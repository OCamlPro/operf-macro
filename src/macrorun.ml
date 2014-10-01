open Macroperf

type copts = {
  max_iter: int;
  output_file: string;
  ignore_out: [`Stdout | `Stderr] list;
}

let with_oc_safe f file =
  let oc = open_out file in
  try
    f oc;
    close_out oc
  with exn ->
    close_out oc;
    raise exn

let with_oc_of_copts f = function
  | {output_file=""; _} -> f stdout
  | {output_file;_} -> with_oc_safe f output_file

let write_res ?(strip=[]) ?file res =
  let res = List.fold_left (fun a s -> Result.strip s a) res strip in
  let res_string = Result.to_string res in

  (* Write the result into stdout, or <file> if specified *)
  (match file with
   | None -> Printf.printf "%s\n" res_string;
   | Some fn ->
       with_oc_safe (fun oc -> Printf.fprintf oc "%s\n" res_string) fn);

  (* Write the result in cache too if cache exists *)
  let rex = Re_pcre.regexp " " in
  let name = res.Result.src.Benchmark.name |> String.trim in
  let name = Re_pcre.substitute ~rex ~subst:(fun _ -> "_") name in
  try
    let cache_dir = XDGBaseDir.Cache.user_dir ~exists:true () in
    let res_file = cache_dir ^ "/operf/macro/" ^ name
                   ^ "/" ^ res.Result.context_id ^ ".result" in
    XDGBaseDir.mkdir_openfile
      (fun fn -> let oc = open_out fn in
        try
          Printf.fprintf oc "%s\n" res_string;
          close_out oc
        with exn ->
          close_out oc;
          raise exn
      ) res_file
  with Not_found -> ()

let write_res_copts copts res = match copts with
  | {output_file=""; ignore_out } -> write_res ~strip:ignore_out res
  | {output_file; ignore_out } -> write_res ~strip:ignore_out ~file:output_file res

(* Generic function to create and run a benchmark *)
let make_bench_and_run copts cmd bench_out topics =
  (* Build the name of the benchmark from the command line, but
     replace " " by "_" *)
  let name = String.concat " " cmd in
  let name_uscore = String.concat "_" cmd in
  let bench =
    Benchmark.make
      ~name:name_uscore
      ~descr:("Benchmark of " ^ name)
      ~cmd
      ~speed:`Fast
      ~topics ()
  in

  (* Write benchmark to file if asked for *)
  (match bench_out with
  | None -> ()
  | Some benchfile ->
      let oc = open_out benchfile in
      Printf.fprintf oc "%s" (Benchmark.to_string bench);
      close_out oc);

  (* Run the benchmark *)
  let res = Runner.run_exn ~max_iter:copts.max_iter bench in

  (* Write the result in the file specified by -o, or stdout and maybe
     in cache as well *)
  write_res_copts copts res

let perf copts cmd evts bench_out =
  (* Separate events from the event list given in PERF format *)
  let rex = Re_pcre.regexp "," in
  let evts = Re_pcre.split ~rex evts in
  let evts = List.map (fun e -> Topic.(Topic (e, Perf))) evts in
  make_bench_and_run copts cmd bench_out evts

let libperf copts cmd evts bench_out =
  let rex = Re_pcre.regexp "," in
  let evts = Re_pcre.split ~rex evts in
  let rex = Re_pcre.regexp "-" in
  let evts = List.map
      (fun s -> s
                |> String.lowercase
                |> String.capitalize
                |> Re_pcre.substitute ~rex ~subst:(fun _ -> "_")
                |> Sexplib.Std.sexp_of_string
                |> fun s -> Topic.(Topic (Perf.Attr.kind_of_sexp s, Libperf))
      ) evts
  in
  make_bench_and_run copts cmd bench_out evts

let time copts cmd bench_out =
  make_bench_and_run copts cmd bench_out
    Topic.[Topic (`Real, Time); Topic (`User, Time); Topic (`Sys, Time)]

let run copts switch selectors =
  let share = Util.Opam.share ?switch () in

  let kind_of_file filename =
    let open Unix in
    try
      let st = Unix.stat filename in
      match st.st_kind with
      | S_REG -> `File
      | S_DIR -> `Directory
      | _     -> `Other_kind
    with Unix_error (ENOENT, _, _) -> `Noent
  in

  let ls dirname =
    let dh = Unix.opendir dirname in
    let rec loop acc =
      match Unix.readdir dh with
      | name -> loop (name::acc)
      | exception End_of_file -> acc
    in loop []
  in

  (* If no selectors, $OPAMROOT/$SWITCH/share/* become the selectors *)
  let selectors = match selectors with
    | [] ->
        let names = ls share in
        let names = List.map (fun n -> Filename.concat share n) names in
        List.filter (fun n -> kind_of_file n = `Directory)
          names
    | selectors -> selectors
  in
  (* If selector is a file, run the benchmark in the file, if it is
     a directory, run all benchmarks in the directory *)
  let rec run_inner selector =
    let run_bench filename =
      let bench_str = Util.File.string_of_file filename in
      let b = Benchmark.of_string bench_str in
      Printf.printf "Running benchmark %s...%!" b.Benchmark.name;
      let res = Runner.run_exn ~max_iter:copts.max_iter b in
      Printf.printf " done.\n%!"; res
    in
    match kind_of_file selector with
    | `Noent ->
        (* Not found, but can be an OPAM package name... *)
        (match kind_of_file Filename.(concat share selector) with
         | `Noent | `File | `Other_kind ->
             Printf.eprintf "Warning: %s is not an OPAM package.\n" selector;
             []
         | `Directory -> run_inner Filename.(concat share selector))
    | `Other_kind ->
        Printf.eprintf "Warning: %s is not a file nor a directory.\n" selector;
        [] (* Do nothing if not file or directory *)
    | `Directory ->
        (* Get a list of .bench files in the directory and run them *)
        let names = ls selector in
        let names =
          List.filter
            (fun n ->
               let len = String.length n in
               len > 6 &&
               kind_of_file Filename.(concat selector n) = `File &&
               String.sub n (len-6) 6 = ".bench")
            names
        in
        List.map run_bench @@ List.map (fun n -> Filename.concat selector n) names
    | `File ->
        List.map run_bench [selector]
  in
  let res = List.map run_inner selectors in
  let res = List.flatten res in
  List.iter (write_res_copts copts) res

let help copts man_format cmds topic = match topic with
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

open Cmdliner

(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [
  `S copts_sect;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
  `S "BUGS"; `P "Report bugs at <http://github.com/OCamlPro/oparf-macro>.";]

let copts output_file ignore_out max_iter =
  { max_iter;
    output_file;
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
  let max_iter =
    let doc = "Maximum number of executions (default: 1000)." in
    Arg.(value & opt int 1000 & info ["l"; "limit"] ~docv:"int" ~docs ~doc) in
  let output_file =
    let doc = "File to write the result to (default: stdout)." in
    Arg.(value & opt string "" & info ["o"; "output"] ~docv:"file" ~docs ~doc) in
  let ignore_out =
    let doc = "Discard program output (default: none)." in
    Arg.(value & opt (list string) [] & info ["discard"] ~docv:"<channel>" ~docs ~doc) in
  Term.(pure copts $ output_file $ ignore_out $ max_iter)

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
  Term.(ret (pure help $ copts_t $ Term.man_format $ Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Macrobenchmarking suite for OCaml." in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "macrorun" ~version:"0.1" ~sdocs:copts_sect ~doc ~man

let perf_cmd =
  let bench_out =
    let doc = "Export the generated bench to file." in
    Arg.(value & opt (some string) None & info ["export"] ~docv:"file" ~doc) in
  let cmd =
    let doc = "Any command you can specify in a shell." in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"<command>" ~doc)
  in
  let evts =
    let doc = "Same as the -e argument of PERF-STAT(1)." in
    Arg.(value & opt string "" & info ["e"; "event"] ~docv:"perf-events" ~doc) in

  let doc = "Macrobenchmark using PERF-STAT(1) (Linux only)." in
  let man = [
    `S "DESCRIPTION";
    `P "Wrapper to the PERF-STAT(1) command."] @ help_secs
  in
  Term.(pure perf $ copts_t $ cmd $ evts $ bench_out),
  Term.info "perf" ~doc ~sdocs:copts_sect ~man

let libperf_cmd =
  let bench_out =
    let doc = "Export the generated bench to file." in
    Arg.(value & opt (some string) None & info ["export"] ~docv:"file" ~doc) in
  let cmd =
    let doc = "Any command you can specify in a shell." in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"<command>" ~doc)
  in
  let evts =
    let doc = "Same as the -e argument of PERF-STAT(1)." in
    Arg.(value & opt string "" & info ["e"; "event"] ~docv:"perf-events" ~doc) in

  let doc = "Macrobenchmark using the ocaml-perf library." in
  let man = [
    `S "DESCRIPTION";
    `P "See <http://github.com/vbmithr/ocaml-perf>."] @ help_secs
  in
  Term.(pure libperf $ copts_t $ cmd $ evts $ bench_out),
  Term.info "libperf" ~doc ~sdocs:copts_sect ~man

let time_cmd =
  let bench_out =
    let doc = "Export the generated bench to file." in
    Arg.(value & opt (some string) None & info ["export"] ~docv:"file" ~doc) in
  let cmd =
    let doc = "Any command you can specify in a shell." in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"<command>" ~doc)
  in
  let doc = "Macrobenchmark measuring time." in
  let man = [
    `S "DESCRIPTION";
    `P "Macrobenchmark measuring time."] @ help_secs
  in
  Term.(pure time $ copts_t $ cmd $ bench_out),
  Term.info "time" ~doc ~sdocs:copts_sect ~man

let run_cmd =
  let switch =
    let doc = "Use the provided OPAM switch instead of using OPAM's current one." in
    Arg.(value & opt (some string) None & info ["switch"] ~docv:"OPAM switch name" ~doc) in
  let selector =
    let doc = "If the argument correspond to a filename, the benchmark \
               is executed from this file, otherwise \
               the argument is treated as an OPAM package. \
               If missing, all OPAM benchmarks installed in the current switch are executed." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|package>" ~doc)
  in
  let doc = "Run macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Run macrobenchmarks from files."] @ help_secs
  in
  Term.(pure run $ copts_t $ switch $ selector),
  Term.info "run" ~doc ~sdocs:copts_sect ~man

let cmds = [help_cmd; run_cmd; perf_cmd; libperf_cmd; time_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
