open Lwt

open Macroperf
open Macroperf_lwt

type copts = {
  output_file: string;
  nb_iter: int;
}

let oc_of_copts = function
  | {output_file=""; _} -> stdout
  | {output_file;_} -> open_out output_file

let perf copts cmd evts bench_out =
  let name = ref "" in
  List.iter (fun c -> name := !name ^ c ^ " ") cmd;
  let name = String.sub !name 0 (String.length !name - 1) in
  let th =
    let bench =
    Benchmark.make
                 ~name
                 ~descr:("Benchmark of ``" ^ name ^
                         "'' avg. over " ^ string_of_int copts.nb_iter ^
                         " iterations.")
                 ~cmd
                 ~nb_iter:copts.nb_iter
                 ~speed:`Fast
                 ~measures:[Topic.Perf evts] ()
    in
    Runner.run_exn bench >|= fun res ->
    Result.to_string res |> fun res ->
    let oc = oc_of_copts copts in
    Printf.fprintf oc "%s\n" res;
    close_out oc;
    match bench_out with
    | None -> ()
    | Some benchfile ->
        let oc = open_out benchfile in
        Printf.fprintf oc "%s" (Benchmark.to_string bench);
        close_out oc
  in Lwt_main.run th

let load copts files =
  let th =
    let inner oc file =
      let bench_str =
        let ic = open_in file in
        try
          let s =
            really_input_string ic @@ in_channel_length ic in
          close_in ic; s
        with exn ->
          close_in ic; raise exn
      in
      bench_str |> Benchmark.of_string |> Runner.run_exn >|= fun res ->
      Result.to_string res |> fun res ->
      Printf.fprintf oc "%s\n" res
    in
    let oc = oc_of_copts copts in
    Lwt_list.iter_s (inner oc) files >|= fun () ->
    close_out oc
  in Lwt_main.run th

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

let load_cmd =
  let filename =
    let doc = "File containing a benchmark description." in
    Arg.(non_empty & pos_all file [] & info [] ~docv:"file" ~doc)
  in
  let doc = "Load macrobenchmarks from files." in
  let man = [
    `S "DESCRIPTION";
    `P "Load macrobenchmarks from files."] @ help_secs
  in
  Term.(pure load $ copts_t $ filename),
  Term.info "load" ~doc ~sdocs:copts_sect ~man

let cmds = [help_cmd; load_cmd; perf_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
