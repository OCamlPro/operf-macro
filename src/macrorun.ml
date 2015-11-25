open Macroperf

module List = struct
  include List
  let filter_map f l =
    List.fold_left (fun a e -> match f e with Some v -> v::a | None -> a) [] l |> List.rev
end

module StringList = struct
  let settrip l = SSet.(of_list l |> elements)
end

module String = struct
  include String
  let prefix s s' =
    length s <= length s' && s = sub s' 0 @@ length s
end

type copts = {
  output: [`Stdout | `File of string | `None];
  ignore_out: [`Stdout | `Stderr] list;
  opamroot: string option;
}

let write_res_copts copts res =
  let res = List.fold_left (fun a s -> Result.strip s a) res copts.ignore_out in

  (* Write the result into stdout, or <file> if specified *)
  (match copts.output with
   | `None -> ()
   | `Stdout -> Result.output_hum stdout res
   | `File fn ->
       try Result.save_hum fn res
       with Sys_error _ -> ()
         (* Sexplib cannot create temporary file, aborting*)
  );

  (* Write the result in cache too if cache exists *)
  let rex = Re_pcre.regexp " " in
  let name = res.Result.bench.Benchmark.name |> String.trim in
  let name = Re_pcre.substitute ~rex ~subst:(fun _ -> "_") name in
  let interactive = copts.output = `None in
  let opamroot = copts.opamroot in
  try
    let res_file =
      Util.FS.(macro_dir / name / res.Result.context_id ^ ".result") in
    XDGBaseDir.mkdir_openfile
      (fun fn ->
         let filename = Filename.chop_extension fn in
         let outputname = filename ^ ".output" in
         Result.save_output outputname res;
         let res = Runner.run_check ?opamroot ~interactive res in
         Result.save_hum fn res) res_file
  with Not_found -> ()

(* Generic function to create and run a benchmark *)
let make_bench_and_run copts cmd topics time_limit =
  let absolute = function
    | [] -> assert false
    | h::t as cmd ->
        if not @@ Sys.file_exists h then invalid_arg @@ Printf.sprintf "%s does not exist" h
        else if not @@ Filename.is_relative h then cmd
        else (Filename.concat (Unix.getcwd ()) h) :: t
  in
  let cmd = absolute cmd in
  let name = Filename.basename @@ List.hd cmd in
  let bench =
    Benchmark.make
      ~name
      ~descr:("Benchmark of " ^ name)
      ~cmd
      ~speed:`Slower
      ~topics ()
  in

  (* Run the benchmark *)
  let interactive = copts.output = `None in
  let res = Runner.run_exn ~interactive ~fixed:false ~time_limit bench in

  (* Write the result in the file specified by -o, or stdout and maybe
     in cache as well *)
  write_res_copts copts res

let perf copts cmd evts =
  let evts = List.map (fun e -> Topic.(Topic (e, Perf))) evts in
  make_bench_and_run copts cmd evts

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

let run copts switch topics selectors skip force fixed time_limit =
  let opamroot = copts.opamroot in
  let switch = match switch with
    | None -> Util.Opam.cur_switch ~opamroot
    | Some switch -> switch in
  let switch = try
      List.hd @@ Util.Opam.switches_matching ?opamroot switch
    with Failure "hd" ->
      Printf.eprintf "Pattern %s do not match any existing switch. Aborting.\n" switch;
      exit 1
  in
  let interactive = copts.output = `None in
  let topics = List.fold_right TSet.add topics TSet.empty in

  let already_run switch b =
    match
      Result.load_conv @@
      Util.FS.(macro_dir / b.Benchmark.name / switch ^ ".result")
    with
    | `Result _ -> true
    | _ -> false
    | exception Sys_error _ -> false
  in

  let files, names = List.partition Sys.file_exists selectors in
  let selectors = match files, names with
    | [], [] -> List.map snd @@ Benchmark.find_installed ?opamroot switch
    | files, [] -> files
    | _ ->
        if skip then
          Benchmark.find_installed ?opamroot ~glob:(`Exclude names) switch
          |> List.map snd
          |> List.append files
        else
          Benchmark.find_installed ?opamroot ~glob:(`Matching names) switch
          |> List.map snd
          |> List.append files
  in
  (* If selector is a file, run the benchmark in the file, if it is
     a directory, run all benchmarks in the directory *)
  let rec run_inner selector =
    let run_bench filename =
      let open Benchmark in
      let b = load_conv_exn filename in
      let b = { b with topics = TSet.union topics b.topics } in
      let already_run = (already_run switch b && not force) in
      let binary_missing =
        try Util.FS.is_file (List.hd b.cmd) <> Some true
        with _ -> true in
      if already_run || binary_missing
      then
        (if interactive then
           let reason = List.fold_left2
               (fun a b s -> if b then s::a else a) []
               [already_run; binary_missing]
               ["already run";
                Printf.sprintf "No binary at path \"%s\"" (List.hd b.cmd)]
           in let reason_str = String.concat ", " reason in
           Printf.printf "Skipping %s (%s)\n" b.name reason_str)
      else
        let res = Runner.run_exn ?opamroot ~context_id:switch ~interactive ~fixed ~time_limit b in
        write_res_copts copts res
    in
    match kind_of_file selector with
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
    | _ -> assert false
  in
  if interactive then
    Printf.printf "Running benchmarks installed in %s...\n" switch;
  List.iter run_inner selectors

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

let list copts switches =
  let print files_names =
      let max_name_len = List.fold_left
          (fun a (n,fn) ->
             let len = String.length n in
             if len > a then len else a)
          0 files_names
      in
      List.iter (fun (n,fn) ->
          Printf.printf "%-*s %s\n" max_name_len n fn
        ) files_names in
  let print_all switches =
    List.iter (fun s ->
        Printf.printf "# %s\n" s;
        print @@ Benchmark.find_installed ?opamroot:copts.opamroot s;
        print_endline ""
      ) switches in
  let switches = match switches with
    | [] -> Util.Opam.switches ~opamroot:copts.opamroot
    | s ->
        StringList.settrip @@
        List.(flatten @@ map Util.Opam.switches_matching s) in
  print_all switches

let output_gnuplot_file oc backend datafile topic nb_cols =
  let plot_line n =
    Printf.sprintf "plot for [i=2:%d:2] '%s' u i:i+1:xtic(1) ti col(i) ls i\n" (2*n) datafile
  in
  let terminal = match backend with
    | `Qt -> {|set terminal qt enhanced font "terminus,14" persist size 1000, 700|}
    | `Svg -> "set terminal svg enhanced\n\
               set output \'result.svg\'"
  in
  let fmt :  (string -> string -> string -> unit, out_channel, unit) format =
    {|set style line 2 lc rgb '#E41A1C' # red
set style line 4 lc rgb '#377EB8' # blue
set style line 6 lc rgb '#4DAF4A' # green
set style line 8 lc rgb '#984EA3' # purple
set style line 10 lc rgb '#FF7F00' # orange
set style line 12 lc rgb '#FFFF33' # yellow
set style line 14 lc rgb '#A65628' # brown
set style line 16 lc rgb '#F781BF' # pink
set style histogram errorbars gap 1 title offset character 0, 0, 0
set style fill solid 1.00 border lt -1
set style data histograms
%s
set datafile separator ','
set boxwidth 0.9 absolute
set key inside right top vertical Right noreverse noenhanced autotitles nobox
set datafile missing ''
set ylabel "normalized %s (less is better)"
set xtics border in scale 0,0 mirror rotate by -45  offset character 0, 0, 0 autojustify
set xtics norangelimit font ",10"
set xtics ()
set title "Benchmarks"
set yrange [ 0. : 1.25 ] noreverse nowriteback
%s|}
  in
  let topic = Re_pcre.substitute ~rex:(Re_pcre.regexp "_") ~subst:(fun _ -> "\\\\_") topic in
  Printf.fprintf oc fmt terminal topic (plot_line nb_cols)

(* [selectors] are bench _names_ *)
let summarize output evts ref_ctx_id pp selectors force ctx_ids no_normalize =
  let evts =
    try List.map Topic.of_string evts
    with Invalid_argument "Topic.of_string" ->
      (Printf.eprintf
         "At least one of the requested topics (-t) is invalid. Exiting.\n";
       exit 1)
  in

  (* [selectors] are directories hopefully containing .summary
     files. *)
  let selectors = match selectors with
    | [] -> SSet.of_list @@ Util.FS.[macro_dir; micro_dir]
    | ss -> List.fold_left
              (fun a s -> try
                  if Sys.is_directory s
                  then SSet.add s a (* selector is a directory, looking for content *)
                  else a (* selector is a file, do nothing *)
                with _ ->
                  (* Not a file nor a dir: benchmark glob expression *)
                  (try
                     let macro_benchs =
                       try Util.FS.(ls ~prefix:true  ~glob:s macro_dir) with _ -> [] in
                     let micro_benchs =
                       try Util.FS.(ls ~prefix:true  ~glob:s micro_dir) with _ -> [] in
                     SSet.union a @@ SSet.of_list (micro_benchs @ macro_benchs)
                   with Sys_error _ -> a)
              )
              SSet.empty ss
  in

  (* Make sure all .result files have an up-to-date corresponding
     .summary *)
  SSet.iter Summary.summarize_dir selectors;

  (* Create the DB *)
  let data = SSet.fold (fun dn db -> DB.of_dir ~acc:db dn)
      selectors DB.empty in

  (* Filter on context ids *)
  let data = match ctx_ids with
    | [] -> data
    | ctx_ids ->
        let res = List.map
            (fun p -> Re_glob.globx ~anchored:true p |> Re.compile) ctx_ids in
        SMap.map
          (SMap.filter
             (fun ctx _ ->
                List.fold_left (fun a re -> Re.execp re ctx || a) false res
             ))
          data in

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
    if no_normalize
    then data
    else
      match ref_ctx_id with
      | "" -> DB2.normalize ~against:`Biggest data
      | context_id -> DB2.normalize ~against:(`Ctx context_id) data
  in
  match pp with
  | `Sexp ->
      (match output with
      | "" -> DB2.output_hum stdout Summary.Aggr.sexp_of_t data
      | fn -> DB2.save_hum fn Summary.Aggr.sexp_of_t data)
  | `Csv ->
      (match output with
       | "" -> ignore @@ DB2.to_csv stdout data
       | fn -> Util.File.with_oc_safe (fun oc -> ignore @@ DB2.to_csv oc data) fn)
  | `Gnuplot backend ->
      let topic = fst @@ TMap.min_binding data |> Topic.to_string in
      let tmp_data, oc_data = Filename.open_temp_file "macrorun" ".data" in
      let nb_ctxs =
        (try let nb_ctxs = DB2.to_csv ~escape_uscore:true
            oc_data data in close_out oc_data; nb_ctxs
         with exn -> (close_out oc_data; raise exn)) in
      let tmp_f, oc = Filename.open_temp_file "macrorun" ".gnu" in
      let () =
        try output_gnuplot_file oc backend tmp_data topic nb_ctxs; close_out oc
        with exn -> (close_out oc; raise exn) in
      let (_:int) = Sys.command @@ Printf.sprintf "gnuplot %s" tmp_f in
      (if output <> "" then
        Util.File.with_oc_safe
          (fun oc -> output_gnuplot_file oc backend tmp_data topic nb_ctxs) output);
      Util.FS.rm_r [tmp_f; tmp_data]

  | _ -> failwith "Not implemented"

let rank copts topics normalize pp context_ids =
  Summary.summarize_dir Util.FS.macro_dir;
  let topics = List.map Topic.of_string topics in

  (* Create database *)
  let data = DB.of_dir Util.FS.macro_dir in

  (* Filter on requested topics *)
  let data = DB.map
      (fun smry -> match topics with
         | [] -> smry
         | topics ->
             Summary.{ smry with data = TMap.filter (fun t _ -> List.mem t topics) smry.data }
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
  | `Stdout -> print_results stdout
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

let copts batch output_file opamroot ignore_out =
  let output =
    if not batch then `None
    else if output_file = "" then `Stdout
    else `File output_file in
  { output;
    opamroot;
    ignore_out=List.map
        (function
          | "stdout" -> `Stdout
          | "stderr" -> `Stderr
          | _ -> invalid_arg "copts"
        )
        ignore_out
  }

let output_file =
  let docs = copts_sect in
  let doc = "File to write the result to (default: stdout)." in
  Arg.(value & opt string "" & info ["o"; "output"] ~docv:"file" ~docs ~doc)

let copts_t =
  let docs = copts_sect in
  let opamroot =
    let doc = "Specify the opam root (default: use OPAMROOT environment \
        variable)" in
    Arg.(value & opt (some string) None & info ["opamroot"] ~docv:"<dir>" ~docs ~doc) in
  let batch =
    let doc = "Run in batch mode, i.e. print result files to stdout \
        instead of printing information about progression." in
    Arg.(value & flag & info ["batch"] ~docs ~doc) in
  let ignore_out =
    let doc = "Discard program output (default: none)." in
    Arg.(value & opt (list string) [] & info ["discard"] ~docv:"<channel>" ~docs ~doc) in
  Term.(pure copts $ batch $ output_file $ opamroot $ ignore_out)

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

let time_limit =
  let doc = "Fixed minimun execution time" in
  Arg.(value & opt float 0. & info ["time-limit"] ~doc)

let perf_cmd =
  let cmd =
    let doc = "Absolute path of an executable and its arguments. \
               If arguments are paths, they must also be absolute \
               (no shell expansion is done). " in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"<command>" ~doc) in
  let evts =
    let doc = "Same as the -e argument of PERF-STAT(1)." in
    Arg.(value & opt (list string) ["cycles"] & info ["e"; "event"] ~docv:"perf-events" ~doc) in
  let doc = "Macrobenchmark using PERF-STAT(1) (Linux only)." in
  let man = [
    `S "DESCRIPTION";
    `P "Wrapper to the PERF-STAT(1) command."] @ help_secs
  in
  Term.(pure perf $ copts_t $ cmd $ evts $ time_limit),
  Term.info "perf" ~doc ~sdocs:copts_sect ~man

let switch =
  let doc = "Look for benchmarks installed in another switch, instead of the current one." in
  Arg.(value & opt (some string) None & info ["s"; "switch"] ~docv:"glob_pat" ~doc)

let run_cmd =
  let force =
    let doc = "Force the execution of benchmarks even if \
               a result file is already present in the file system." in
    Arg.(value & flag & info ["f"; "force"] ~doc) in
  let skip =
    let doc = "Inverse benchmark selection. (only when arguments are package globs)." in
    Arg.(value & flag & info ["skip"] ~docv:"benchmark list" ~doc)
  in
  let fixed =
    let doc = "Fixed execution time." in
    Arg.(value & flag & info ["fixed"] ~doc)
  in
  let topics =
    let doc = "Additionnal values measured during the benchmark evaluation" in
    let topic_parser s =
      try `Ok (Topic.of_string s)
      with _ -> `Error (Printf.sprintf "%s is not a valid topic" s)
    in
    let topic_printer ppf t =
      Format.pp_print_string ppf (Topic.to_string t)
    in
    let converter = topic_parser, topic_printer in
    Arg.(value & opt_all converter [] & info ["t"; "topic"] ~docv:"evaluated topics" ~doc)
  in
  let selector =
    let doc = "If the argument is the path to an existing file, \
               it is taken as a benchmark file (.bench), otherwise \
               the argument is treated as an OPAM package shell pattern. \
               If missing, all OPAM benchmarks installed in \
               the current switch (or the one specified) are executed." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|package_glob>" ~doc)
  in
  let doc = "Run macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Run macrobenchmarks from files."] @ help_secs
  in
  Term.(pure run $ copts_t $ switch $ topics $ selector $ skip $ force $ fixed $ time_limit),
  Term.info "run" ~doc ~sdocs:copts_sect ~man


let list_cmd =
  let switches =
    let doc = "List installed benchmarks for this switch." in
    Arg.(value & pos_all string [] & info [] ~docv:"<switch>" ~doc) in
  let doc = "List installed OPAM benchmarks." in
  let man = [
    `S "DESCRIPTION";
    `P "List installed OPAM benchmarks."] @ help_secs
  in
  Term.(pure list $ copts_t $ switches),
  Term.info "list" ~doc ~sdocs:copts_sect ~man

(* Arguments common to summarize, rank. *)

let topics =
  let doc = "Select the topics to summarize. \
             This command understand gc stats, \
             perf events, times... (default: all topics)." in
  Arg.(value & opt (list string) [] & info ["t"; "topics"] ~docv:"topics" ~doc)

let switches =
  let doc = "compiler switches to select" in
  Arg.(value & opt (list string) [] & info ["s";"switches"] ~docv:"switch name" ~doc)

let normalize =
  let doc = "Normalize against the value of a context_id (compiler)." in
  Arg.(value & opt string "" & info ["n"; "normalize"] ~docv:"context_id" ~doc)

let backend =
  let doc = "Select backend (one of 'sexp','csv','qt','svg')." in
  let enum_f =
    ["sexp", `Sexp; "csv", `Csv; "qt", `Gnuplot `Qt; "svg", `Gnuplot `Svg]
  in
  Arg.(value & opt (enum enum_f) `Sexp & info ["b"; "backend"] ~doc)

let no_normalize =
  let doc = "Don't normalize values." in
  Arg.(value & flag & info ["no-normalize"] ~doc)

let summarize_cmd =
  let force =
    let doc = "Force rebuilding the summary files." in
    Arg.(value & flag & info ["f"; "force"] ~doc) in
  let selector =
    let doc = "If the argument is the path to an existing file, it is taken \
               as a .result file, otherwise it is treated as \
               a shell pattern (glob) matching a benchmark name. \
               If missing, all results of previously ran benchmarks are used." in
    Arg.(value & pos_all string [] & info [] ~docv:"<file|benchmark_glob>" ~doc)
  in
  let doc = "Produce a summary of the result of the desired benchmarks." in
  let man = [
    `S "DESCRIPTION";
    `P "Produce a summary of the result of the desired benchmarks."] @ help_secs
  in
  Term.(pure summarize $ output_file $ topics
        $ normalize $ backend $ selector $ force $ switches $ no_normalize),
  Term.info "summarize" ~doc ~man

let rank_cmd =
  let doc = "Produce an aggregated performance index for the desired compilers." in
  let man = [
    `S "DESCRIPTION";
    `P "Produce an aggregated performance index for the desired compilers."] @ help_secs
  in
  Term.(pure rank $ copts_t $ topics $ normalize $ backend $ switches,
        info "rank" ~doc ~man)

let cmds = [help_cmd; run_cmd; summarize_cmd; rank_cmd;
            list_cmd; perf_cmd]

let () = match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
