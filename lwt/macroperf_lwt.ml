open Macroperf

module Generic = struct
  let string_of_ic ic = really_input_string ic @@ in_channel_length ic

  let lines_of_ic ic =
    let rec get_line acc = match input_line ic with
      | line -> get_line (line::acc)
      | exception End_of_file -> acc
    in get_line []

  let string_of_file filename =
    let ic = open_in filename in
    try
      let res = string_of_ic ic in close_in ic; res
    with exn ->
      close_in ic; raise exn

  let lines_of_file filename =
    let ic = open_in filename in
    try
      let res = lines_of_ic ic in close_in ic; res
    with exn ->
      close_in ic; raise exn

  let stdout_of_cmd cmd_string =
    let ic = Unix.open_process_in cmd_string in
    try
      let res = string_of_ic ic in Unix.close_process_in ic, res
    with exn ->
      let _ = Unix.close_process_in ic in raise exn

  type 'a process_ops = {
    new_f: unit -> 'a;
    start_f: 'a -> unit;
    stop_f: 'a -> unit;
    state_f: 'a -> (Topic.t * Measure.t) list
  }

  let with_process_exn ?env ?timeout cmd p_ops =
    let tmp_stdout_name = Filename.temp_file "ocaml-perf" "stdout" in
    let tmp_stderr_name = Filename.temp_file "ocaml-perf" "stderr" in
    let tmp_stdout =
      Unix.(openfile tmp_stdout_name [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
    let tmp_stderr =
      Unix.(openfile tmp_stderr_name [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
    let state = p_ops.new_f () in
    p_ops.start_f state;
    match Unix.fork () with
    | 0 ->
        (* child *)
        Unix.(dup2 tmp_stdout stdout; close tmp_stdout);
        Unix.(dup2 tmp_stderr stderr; close tmp_stderr);
        (match env with
         | None -> Unix.execvp (List.hd cmd) (Array.of_list cmd)
         | Some env -> Unix.execvpe (List.hd cmd)
                         (Array.of_list cmd) (Array.of_list env))
    | n ->
        (* parent *)
        (* Setup an alarm if timeout is specified. The alarm signal
           handles do nothing, but this will make waitpid fail with
           EINTR, unblocking the program. *)
        let (_:int) = match timeout with None -> 0 | Some t -> Unix.alarm t in
        Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
        let _, process_status = Unix.waitpid [] n in
        p_ops.stop_f state;
        Unix.(close tmp_stdout; close tmp_stderr);
        let res =
          Execution.{
            process_status;
            stdout = string_of_file tmp_stdout_name;
            stderr = string_of_file tmp_stderr_name;
            data = p_ops.state_f state;
          }
        in
        Unix.(unlink tmp_stdout_name; unlink tmp_stderr_name);
        res

  let with_process ?env ?timeout cmd p_ops =
    try `Ok (with_process_exn ?env ?timeout cmd p_ops)
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
    | exn -> `Exn exn

  let run_n f n =
    let rec run_n acc = function
      | 0 -> acc
      | n -> let exec = f () in run_n (exec::acc) (n-1)
    in run_n [] n

end

module Perf_wrapper = struct
  include Generic

  let run_once ?env cmd evts =
    let perf_cmdline = ["perf"; "stat"; "-x,"; ] in
    let perf_cmdline = match evts with
      | [] -> perf_cmdline
      | _ -> perf_cmdline @ ["-e"; String.concat "," evts] in
    let cmd = perf_cmdline @ cmd in
    let env = match env with
      | None -> [|"LANG=C"|]
      | Some env -> Array.of_list @@ "LANG=C"::env in
    let cmd_string = String.concat " " cmd in
    let p_stdout, p_stdin, p_stderr = Unix.open_process_full cmd_string env in
    try
      let stdout_string = string_of_ic p_stdout in
      let stderr_lines = lines_of_ic p_stderr in
      let process_status =  Unix.close_process_full (p_stdout, p_stdin, p_stderr) in
      let rex = Re.(str "," |> compile) in
      let stderr_lines = List.map (Re_pcre.split ~rex) stderr_lines in
      `Ok Execution.{
          process_status;
          stdout=stdout_string;
          stderr=""; (* Perf writes its result on stderr... *)
          data=(List.fold_left
                  (fun acc l -> match l with
                     | [v;"";event; ]
                     | [v;"";event; _] ->
                         (Topic.(Topic (event, Perf)), Measure.of_string v)::acc
                     | l ->
                         Printf.printf
                           "Ignoring perf result line [%s]" (String.concat "," l);
                         acc
                  )
                  [] stderr_lines);
        }
    with exn ->
      let _ = Unix.close_process_full (p_stdout, p_stdin, p_stderr) in raise exn

  let run ?env ?(nb_iter=1) cmd evts =
    run_n (fun () -> run_once ?env cmd evts) nb_iter
end

module Time_wrapper = struct
  include Generic

  (* TODO: implement `Sys and `User times. *)
  let run_once ?env ?timeout cmd times =
    let p_ops = {
      new_f = (fun () -> ref 0.);
      start_f = (fun r -> r := Unix.gettimeofday ());
      stop_f = (fun r -> let now = Unix.gettimeofday () in r := !r -. now);
      state_f = (fun r -> [Topic.(Topic (`User, Time)), `Float !r]);
    } in
    with_process ?env ?timeout cmd p_ops

  let run ?env ?timeout ?(nb_iter=1) cmd times =
    run_n (fun () -> run_once ?env ?timeout cmd times) nb_iter
end

module Libperf_wrapper = struct
  include Generic

  let run_once ?env cmd attrs =
    let open Perf in
    let attrs = List.map Perf.Attr.make attrs in
    with_process ?env cmd attrs |> function
    | `Ok {process_status; stdout; stderr; data;} ->
        let data = List.map (fun (k, v) ->
            Topic.(Topic (k, Libperf)), `Int v) data in
        `Ok Execution.{ process_status; stdout; stderr; data; }
    | `Timeout -> `Timeout
    | `Exn e -> `Exn e

  let run ?env ?(nb_iter=1) cmd evts =
    run_n (fun () -> run_once ?env cmd evts) nb_iter
end

module Runner = struct
  type execs = {
    time: Topic.time list;
    gc: Topic.gc list;
    libperf: Perf.Attr.kind list;
    perf: string list;
  }

  let run_exn ?nb_iter ?topics b =
    let open Benchmark in

    let nb_iter = match nb_iter with
      | None -> b.nb_iter
      | Some nb_iter -> nb_iter in
    let topics = match topics with
      | None -> b.topics
      | Some ts -> ts
    in

    (* Transform individial topics into a list of executions *)
    let execs =
      let open Topic in
      List.fold_left
        (fun a -> function
           | Topic (t, Time) -> { a with time=t::a.time }
           | Topic (t, Gc) -> { a with gc=t::a.gc }
           | Topic (t, Libperf) -> { a with libperf=t::a.libperf }
           | Topic (t, Perf) -> { a with perf=t::a.perf }
        )
        {time=[]; gc=[]; libperf=[]; perf=[];}
        topics in

    let run_execs { time; gc; libperf; perf; } b =
      (* Launch the executions only if the list of topics is
         non-empty. *)
      let time_res = match time with
        | [] -> []
        | time -> Time_wrapper.(run ?env:b.env ~nb_iter b.cmd time) in
      let libperf_res = match libperf with
        | [] -> []
        | libperf -> Libperf_wrapper.(run ?env:b.env ~nb_iter b.cmd libperf) in
      let perf_res = match perf with
        | [] -> []
        | perf -> Perf_wrapper.(run ?env:b.env ~nb_iter b.cmd perf) in
      (time_res @ libperf_res @ perf_res)
    in

    let execs = run_execs execs b in
    Result.make ~context_id:"unknown" ~src:b ~execs ()

  let run ?nb_iter ?topics b =
    try Some (run_exn ?nb_iter ?topics b) with _ -> None
end
