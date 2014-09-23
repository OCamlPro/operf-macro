open Lwt

open Macroperf
open Macroperf_lwt

type copts = {
  output_file: string;
  nb_iter: int;
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

let write_res ?file res =
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
  | {output_file=""; _} -> write_res res
  | {output_file;_} -> write_res ~file:output_file res

(* Generic function to create and run a benchmark *)
let make_bench_and_run copts cmd bench_out measures =
  (* Build the name of the benchmark from the command line, but
     replace " " by "_" *)
  let name = String.concat " " cmd in
  let name_uscore = String.concat "_" cmd in
  let th =
    let bench =
    Benchmark.make
                 ~name:name_uscore
                 ~descr:("Benchmark of ``" ^ name ^
                         "'' avg. over " ^ string_of_int copts.nb_iter ^
                         " iterations.")
                 ~cmd
                 ~nb_iter:copts.nb_iter
                 ~speed:`Fast
                 ~measures ()
    in
    Runner.run_exn bench >|= fun res ->

    (* Write the result in the file specified by -o, or stdout and
       maybe in cache as well *)
    write_res_copts copts res;

    match bench_out with
    | None -> ()
    | Some benchfile ->
        let oc = open_out benchfile in
        Printf.fprintf oc "%s" (Benchmark.to_string bench);
        close_out oc
  in Lwt_main.run th

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

let run copts files =
  let th =
    let run_inner file =
      let bench_str =
        let ic = open_in file in
        try
          let s =
            really_input_string ic @@ in_channel_length ic in
          close_in ic; s
        with exn ->
          close_in ic; raise exn
      in
      bench_str |> Benchmark.of_string |> Runner.run_exn
    in
    Lwt_list.map_s run_inner files >|= fun res ->
    List.iter (write_res_copts copts) res
  in
  Lwt_main.run th

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

let copts output_file nb_iter = { output_file; nb_iter; }

let copts_t =
  let docs = copts_sect in
  let output_file =
    let doc = "File to write the result to (default: stdout)." in
    Arg.(value & opt string "" & info ["o"; "output"] ~docv:"file" ~docs ~doc) in
  let nb_iter =
    let doc = "Number of iterations (default: 1)." in
    Arg.(value & opt int 1 & info ["r"; "repeat"] ~docv:"<n>" ~docs ~doc)
  in
  Term.(pure copts $ output_file $ nb_iter)

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
    let doc = "Equivalent to the -e argument of PERF-STAT(1)." in
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
    let doc = "Equivalent to the -e argument of PERF-STAT(1)." in
    Arg.(value & opt string "" & info ["e"; "event"] ~docv:"perf-events" ~doc) in

  let doc = "Macrobenchmark using PERF-STAT(1) (Linux only)." in
  let man = [
    `S "DESCRIPTION";
    `P "Wrapper to the PERF-STAT(1) command."] @ help_secs
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
  let filename =
    let doc = "File containing a benchmark description." in
    Arg.(non_empty & pos_all file [] & info [] ~docv:"file" ~doc)
  in
  let doc = "Run macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Run macrobenchmarks from files."] @ help_secs
  in
  Term.(pure run $ copts_t $ filename),
  Term.info "run" ~doc ~sdocs:copts_sect ~man

let cmds = [help_cmd; run_cmd; perf_cmd; libperf_cmd; time_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
