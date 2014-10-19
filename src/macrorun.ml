open Macroperf

type copts = {
  output: [`Channel of out_channel | `File of string | `None];
  ignore_out: [`Stdout | `Stderr] list;
}

let write_res_copts copts res =
  let res = List.fold_left (fun a s -> Result.strip s a) res copts.ignore_out in

  (* Write the result into stdout, or <file> if specified *)
  (match copts.output with
   | `None -> ()
   | `Channel oc -> Sexplib.Sexp.output_hum oc @@ Result.sexp_of_t res
   | `File fn ->
       try Sexplib.Sexp.save_hum fn @@ Result.sexp_of_t res
       with Sys_error _ -> ()
         (* Sexplib cannot create temporary file, aborting*)
  );

  (* Write the result in cache too if cache exists *)
  let rex = Re_pcre.regexp " " in
  let name = res.Result.src.Benchmark.name |> String.trim in
  let name = Re_pcre.substitute ~rex ~subst:(fun _ -> "_") name in
  try
    let res_file =
      Util.FS.(cache_dir / name / res.Result.context_id ^ ".result") in
    XDGBaseDir.mkdir_openfile
      (fun fn -> Sexplib.Sexp.save_hum fn @@ Result.sexp_of_t res) res_file
  with Not_found -> ()

(* Generic function to create and run a benchmark *)
let make_bench_and_run copts cmd bench_out topics =
  (* Build the name of the benchmark from the command line, but
     replace " " by "_" *)
  let cmd = match cmd with
    | [] -> assert false
    | h::t when Util.FS.exists h -> cmd
    | h::t -> (Util.Cmd.path_of_exe h)::t
  in
  let name = Filename.basename @@ List.hd cmd in
  let bench =
    Benchmark.make
      ~name
      ~descr:("Benchmark of " ^ name)
      ~cmd
      ~speed:`Fast
      ~topics ()
  in

  (* Write benchmark to file if asked for *)
  (match bench_out with
  | None -> ()
  | Some benchfile ->
      Sexplib.Sexp.save_hum benchfile @@ Benchmark.sexp_of_t bench);

  (* Run the benchmark *)
  let interactive = copts.output = `None in
  let res = Runner.run_exn ~interactive bench in

  (* Write the result in the file specified by -o, or stdout and maybe
     in cache as well *)
  write_res_copts copts res

let perf copts cmd evts bench_out =
  (* Separate events from the event list given in PERF format *)
  let evts = List.map (fun e -> Topic.(Topic (e, Perf))) evts in
  make_bench_and_run copts cmd bench_out evts

let libperf copts cmd evts bench_out =
  let rex = Re_pcre.regexp "-" in
  let evts = try List.map
      (fun s -> s
                |> String.lowercase
                |> String.capitalize
                |> Re_pcre.substitute ~rex ~subst:(fun _ -> "_")
                |> Sexplib.Std.sexp_of_string
                |> fun s -> Topic.(Topic (Perf.Attr.Kind.t_of_sexp s, Libperf))
      ) evts
    with Invalid_argument "kind_of_sexp" ->
      (Printf.eprintf
         "At least one of the requested topics (-e) is invalid. Exiting.\n";
       exit 1)
  in
  make_bench_and_run copts cmd bench_out evts

let kind_of_file filename =
  let open Unix in
  try
    let st = Unix.stat filename in
    match st.st_kind with
    | S_REG -> `File
    | S_DIR -> `Directory
    | _     -> `Other_kind
  with Unix_error (ENOENT, _, _) -> `Noent

let is_benchmark_file filename =
  kind_of_file filename = `File &&
  Filename.check_suffix filename ".bench"

let run copts switch context_id selectors =
  let share = Util.Opam.share ?switch () in
  let interactive = copts.output = `None in

  (* If no selectors, $OPAMROOT/$SWITCH/share/* become the selectors *)
  let infered_selectors = match selectors with
    | [] ->
        let names = Util.FS.ls share in
        let names = List.map (fun n -> Filename.concat share n) names in
        List.filter (fun n -> kind_of_file n = `Directory)
          names
    | selectors -> selectors
  in
  (* If selector is a file, run the benchmark in the file, if it is
     a directory, run all benchmarks in the directory *)
  let rec run_inner selector =
    let run_bench filename =
      let b = Util.File.sexp_of_file_exn filename Benchmark.t_of_sexp in
      let res = Runner.run_exn ?context_id ~interactive b in
      write_res_copts copts res
    in
    match kind_of_file selector with
    | `Noent ->
        (* Not found, but can be an OPAM package name... *)
        (match kind_of_file Filename.(concat share selector) with
         | `Noent | `File | `Other_kind ->
             Printf.eprintf "Warning: No benchmark found in %s.\n"
               Filename.(concat share selector)
         | `Directory -> run_inner Filename.(concat share selector))
    | `Other_kind ->
        Printf.eprintf "Warning: %s is not a file nor a directory.\n" selector
    | `Directory ->
        (* Get a list of .bench files in the directory and run them *)
        let benchs = Util.FS.ls selector in
        if interactive && benchs = [] && selectors <> [] then
          Printf.printf "No benchmark files (*.bench) found in %s.\n" selector
        else
          List.(map (Filename.concat selector) benchs
                |> filter is_benchmark_file
                |> iter run_bench)
    | `File ->
        List.iter run_bench [selector]
  in
  List.iter run_inner infered_selectors

let help man_format cmds topic = match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
      let topics = "topics" :: "patterns" :: "environment" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
      | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
      | `Ok t ->
          let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
          `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

let list switch =
  let share = Util.Opam.share ?switch () in
  Util.FS.ls share
  |> List.map (fun n -> Filename.concat share n)
  |> List.filter (fun n -> kind_of_file n = `Directory)
  |> List.iter
    (fun selector ->
       Util.FS.ls selector
       |> List.map (Filename.concat selector)
       |> List.filter is_benchmark_file
       |> List.iter (fun s -> Format.printf "%s@." s))

(* [selectors] are bench _names_ *)
let summarize copts evts normalize csv selectors force =
  let evts =
    try List.map Topic.of_string evts
    with Invalid_argument "Topic.of_string" ->
      (Printf.eprintf
         "At least one of the requested topics (-e) is invalid. Exiting.\n";
       exit 1)
  in

  (* [selectors] are directories hopefully containing .summary
     files. *)
  let selectors = match selectors with
    | [] -> [Util.FS.cache_dir]
    | ss -> List.fold_left
              (fun a s -> try
                  if Sys.is_directory s
                  then s::a (* selector is a directory, looking for content *)
                  else a (* selector is a file, do nothing *)
                with Sys_error _ ->
                  (* Not a file nor a dir: benchmark name *)
                  (try
                     if Sys.is_directory Util.FS.(cache_dir / s) then
                       Util.FS.(cache_dir / s)::a
                     else a
                   with Sys_error _ -> a)
              )
              [] ss
  in

  (* Make sure all .result files have an up-to-date corresponding
     .summary *)
  List.iter Summary.summarize_dir selectors;

  (* Create the DB *)
  let data = List.fold_left (fun db dn -> DB.of_dir ~acc:db dn)
      DB.empty selectors in

  (* Filter on requested evts *)
  let data = DB.map
      (fun smry -> match evts with
         | [] -> smry
         | evts ->
             Summary.{ smry with data = TMap.filter (fun t _ -> List.mem t evts) smry.data }
      )
      data in

  (* Create the DB2 from DB *)
  let data = DB.fold_data
      (fun bench context_id topic measure a ->
         DB2.add topic bench context_id measure a
      )
      data DB2.empty in

  let data =
    (match normalize with
     | None -> data
     | Some "" -> DB2.normalize data
     | Some context_id -> DB2.normalize ~context_id data)
  in
  if not csv then
    match copts.output with
    | `None -> Sexplib.Sexp.output_hum stdout @@ DB2.sexp_of_t Summary.Aggr.sexp_of_t data
    | `Channel oc -> Sexplib.Sexp.output_hum oc @@ DB2.sexp_of_t Summary.Aggr.sexp_of_t data
    | `File fn -> Sexplib.Sexp.save_hum fn @@ DB2.sexp_of_t Summary.Aggr.sexp_of_t data
  else
    match copts.output with
    | `None -> DB2.to_csv stdout data
    | `Channel oc -> DB2.to_csv oc data
    | `File fn -> Util.File.with_oc_safe (fun oc -> DB2.to_csv oc data) fn

let rank copts evts normalize csv context_ids =
  Summary.summarize_dir Util.FS.cache_dir;
  let evts = List.map Topic.of_string evts in

  (* Create database *)
  let data = DB.of_dir Util.FS.cache_dir in

  (* Filter on requested evts *)
  let data = DB.map
      (fun smry -> match evts with
         | [] -> smry
         | evts ->
             Summary.{ smry with data = TMap.filter (fun t _ -> List.mem t evts) smry.data }
      )
      data in

  (* Filter on requested context_ids. If unspecified, take all. *)
  let data =
    if context_ids = [] then data
    else
      let context_ids = SSet.of_list context_ids in
      SMap.map
        (fun ctxmap -> SMap.filter (fun k _ -> SSet.mem k context_ids) ctxmap)
        data
  in

  (* Flip the order bench, context_id in the DB. *)
  let data = DB.fold (fun b c tmap -> DB.add c b tmap) data DB.empty in

  let () = if data = DB.empty then exit 0 else () in

  (* Now data is ordered context_id, bench, topics map *)
  (* Compute the intersection of benchmarks done *)
  let common_benchs =
    let init_acc = SMap.(SSet.of_list @@ List.map fst @@ bindings @@ snd @@ min_binding data) in
    SMap.fold (fun k v a ->
        SSet.(inter a @@ of_list @@ List.map fst @@ SMap.bindings v)
      )
      data init_acc in

  let () = if common_benchs = SSet.empty then
      (Printf.eprintf
         "Impossible to rank the requested compilers, because the intersection \
          of benchmarks that have been run on each compiler is empty.\n";
       exit 1
      ) else () in

  (* Filter on common benchs *)
  let data = SMap.map
      (fun benchmap ->
         SMap.filter (fun k _ -> SSet.mem k common_benchs) benchmap)
      data
  in

  (* Now data contains exactly what we want, i.e. the ctx_ids we
     want / the common benchmarks / the topics we selected: generate
     results *)

  let data = DB.map
      (fun smry ->
         let open Summary in
         let ss = TMap.bindings smry.data in
         let ss = List.map (fun (_, aggr) -> aggr.Aggr.mean) ss in
         Statistics.geometric_mean ss, smry.weight
      )
      data
  in
  let data = SMap.map
      (fun benchmap ->
         SMap.fold
           (fun bench (mean, weight) (prod, sumw) ->
              prod *. mean, sumw +. weight
           )
           benchmap (1.,0.))
      data
  in
  let print_results oc =
    SMap.iter
      (fun k v -> Printf.fprintf oc "%s,%f\n" k v)
    @@
    SMap.map (fun (prod, sumw) -> prod ** (1. /. sumw)) data
  in
  match copts.output with
  | `None -> print_results stdout
  | `Channel oc -> print_results oc
  | `File fn -> Util.File.with_oc_safe print_results fn

open Cmdliner

(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [
  `S copts_sect;
  `P "These options are common to some commands (including this one).";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
  `S "BUGS"; `P "Report bugs at <http://github.com/OCamlPro/oparf-macro>.";]

let copts interactive output_file ignore_out =
  let output =
    if interactive then `None
    else if output_file = "" then `Channel stdout
    else `File output_file in
  { output;
    ignore_out=List.map
        (function
          | "stdout" -> `Stdout
          | "stderr" -> `Stderr
          | _ -> invalid_arg "copts"
        )
        ignore_out
  }

let copts_t =
  let docs = copts_sect in
  let interactive =
    let doc = "Run in interactive mode, i.e. do not print result files to screen\
               but rather print information about progression." in
    Arg.(value & flag & info ["i";"interactive"] ~docs ~doc) in
  let output_file =
    let doc = "File to write the result to (default: stdout)." in
    Arg.(value & opt string "" & info ["o"; "output"] ~docv:"file" ~docs ~doc) in
  let ignore_out =
    let doc = "Discard program output (default: none)." in
    Arg.(value & opt (list string) [] & info ["discard"] ~docv:"<channel>" ~docs ~doc) in
  Term.(pure copts $ interactive $ output_file $ ignore_out)

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "Display help about macroperf and macroperf commands." in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about macroperf commands and other subjects..."] @ help_secs
  in
  Term.(ret (pure help $ Term.man_format $ Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Macrobenchmarking suite for OCaml." in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "macrorun" ~version:"0.1" ~sdocs:copts_sect ~doc ~man

(* Common arguments to perf_cmd, libperf_cmd *)
let bench_out =
  let doc = "Export the generated bench to file." in
  Arg.(value & opt (some string) None & info ["export"] ~docv:"file" ~doc)

let cmd =
  let doc = "Any command you can specify in a shell." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"<command>" ~doc)

let evts =
  let doc = "Same as the -e argument of PERF-STAT(1)." in
  Arg.(value & opt (list string) ["cycles"] & info ["e"; "event"] ~docv:"perf-events" ~doc)

let perf_cmd =
  let doc = "Macrobenchmark using PERF-STAT(1) (Linux only)." in
  let man = [
    `S "DESCRIPTION";
    `P "Wrapper to the PERF-STAT(1) command."] @ help_secs
  in
  Term.(pure perf $ copts_t $ cmd $ evts $ bench_out),
  Term.info "perf" ~doc ~sdocs:copts_sect ~man

let libperf_cmd =
  let doc = "Macrobenchmark using the ocaml-perf library." in
  let man = [
    `S "DESCRIPTION";
    `P "See <http://github.com/vbmithr/ocaml-perf>."] @ help_secs
  in
  Term.(pure libperf $ copts_t $ cmd $ evts $ bench_out),
  Term.info "libperf" ~doc ~sdocs:copts_sect ~man

let switch =
  let doc = "Look for benchmarks installed in another switch, instead of the current one." in
  Arg.(value & opt (some string) None & info ["s"; "switch"] ~docv:"string" ~doc)

let run_cmd =
  let context_id =
    let doc = "Use the specified context_id when writing benchmark results, \
              instead of the switch name." in
    Arg.(value & opt (some string) None & info ["c"; "cid"] ~docv:"string" ~doc)
  in
  let selector =
    let doc = "If the argument correspond to a filename, the benchmark \
               is executed from this file, otherwise \
               the argument is treated as an OPAM package. \
               If missing, all OPAM benchmarks installed in \
               the current switch (or the one specified) are executed." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|package>" ~doc)
  in
  let doc = "Run macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Run macrobenchmarks from files."] @ help_secs
  in
  Term.(pure run $ copts_t $ switch $ context_id $ selector),
  Term.info "run" ~doc ~sdocs:copts_sect ~man

let list_cmd =
  let doc = "List installed OPAM benchmarks." in
  let man = [
    `S "DESCRIPTION";
    `P "List installed OPAM benchmarks."] @ help_secs
  in
  Term.(pure list $ switch),
  Term.info "list" ~doc ~sdocs:copts_sect ~man

(* Arguments common to summarize, rank. *)

let generalized_evts =
  let doc = "Select the topic to summarize. \
             This command understand gc stats, \
             perf events, times... (default: all topics)." in
  Arg.(value & opt (list string) [] & info ["e"; "event"] ~docv:"evts" ~doc)

let normalize =
  let doc = "Normalize against the value of a context_id (compiler)." in
  Arg.(value & opt ~vopt:(Some "") (some string) None &
       info ["n"; "normalize"] ~docv:"context_id" ~doc)

let csv =
  let doc = "Output in CSV format." in
  Arg.(value & flag & info ["csv"] ~docv:"boolean" ~doc)

let summarize_cmd =
  let force =
    let doc = "Force rebuilding the summary files." in
    Arg.(value & flag & info ["f"; "force"] ~doc) in
  let selector =
    let doc = "If the argument correspond to a file, it is taken \
               as a .result file, otherwise the argument is treated as \
               a benchmark name. \
               If missing, all results of previously ran benchmarks are used." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|name>" ~doc)
  in
  let doc = "Produce a summary of the result of the desired benchmarks." in
  let man = [
    `S "DESCRIPTION";
    `P "Produce a summary of the result of the desired benchmarks."] @ help_secs
  in
  Term.(pure summarize $ copts_t $ generalized_evts $ normalize $ csv $ selector $ force),
  Term.info "summarize" ~doc ~man

let rank_cmd =
  let context_ids =
    let doc = "context_ids to rank" in
    Arg.(value & pos_all string [] & info [] ~docv:"string" ~doc) in
  let doc = "Produce an aggregated performance index for the desired compilers." in
  let man = [
    `S "DESCRIPTION";
    `P "Produce an aggregated performance index for the desired compilers."] @ help_secs
  in
  Term.(pure rank $ copts_t $ generalized_evts $ normalize $ csv $ context_ids,
        info "rank" ~doc ~man)

let cmds = [help_cmd; run_cmd; summarize_cmd; rank_cmd;
            list_cmd; perf_cmd; libperf_cmd]

let () = match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
