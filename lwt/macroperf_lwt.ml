open Lwt
open Macroperf

let section = Lwt_log.Section.make "macroperf_lwt"

module Wrapper_common = struct

  let run_n f n =
    let rec run_n acc = function
      | 0 -> return acc
      | n -> f () >>= fun exec -> run_n (exec::acc) (n-1)
    in run_n [] n
end

module Perf_wrapper = struct
  include Wrapper_common

  let run_once ?env cmd evts =
    (* perf does not care about PATH *)
    (match cmd with
     | [] -> fail (Invalid_argument "empty command")
     | c::args ->
         (Lwt_process.with_process_in
            ("", [|"/bin/sh"; "-c"; Printf.sprintf "command -v %s" c|])
            (fun pi -> pi#status >>= function
               | Unix.WEXITED 0 -> Lwt_io.read_line pi#stdout
               | _ -> fail (Invalid_argument (Printf.sprintf "%s does not exist" c))
            )
         ) >>= fun abs_cmd ->
         return (abs_cmd::args))
    >>= fun cmd ->
    let perf_cmdline = ["perf"; "stat"; "-x,"; ] in
    let perf_cmdline = match evts with
      | [] -> perf_cmdline
      | _ -> perf_cmdline @ ["-e"; String.concat "," evts] in
    let cmd = "", Array.of_list @@ perf_cmdline @ cmd in
    let env = match env with
      | None -> Some [|"LANG=C"|]
      | Some env -> Some (Array.of_list @@ "LANG=C"::env) in

    let produce_result pfull =
      let rex = Re.(str "," |> compile) in
      let rec drain_stderr acc =
        catch
          (fun () -> Lwt_io.read_line pfull#stderr)
          (fun _ -> return "") >>= fun line ->
        match line with
        | "" -> return acc
        | l -> drain_stderr ((Re_pcre.split ~rex line)::acc)
      in
      Lwt_io.read pfull#stdout >>= fun stdout ->
      pfull#status >>= function
      | Unix.WEXITED return_value ->
          drain_stderr [] >|= fun res ->
          Result.Execution.{
            return_value;
            stdout;
            stderr=""; (* Perf writes its result on stderr... *)
            data=(List.fold_left
                    (fun acc l -> match l with
                       | [v;"";event; ]
                       | [v;"";event; _] ->
                           (Topic.(Topic (event, Perf)), Result.Measure.of_string  v)::acc
                       | l ->
                           Lwt_log.ign_warning_f ~section
                             "Ignoring perf result line [%s]" (String.concat "," l);
                           acc
                    )
                    [] res);
          }
      | _ -> fail (Failure "perf has been killed")
    in
    Lwt_process.with_process_full ?env cmd produce_result

  let run ?env ?(nb_iter=1) cmd evts =
    run_n (fun () -> run_once ?env cmd evts) nb_iter
end

module Time_wrapper = struct
  include Wrapper_common

  (* I'm not sure of the overhead introduced in the measurement of
     time real, but it should not be relied upon. *)
    let run_once ?env cmd times =
      let env = match env with
        | None -> None
        | Some e -> Some (Array.of_list e) in
      let produce_result t_start p =
        p#rusage >>= fun rusage ->
        let t_end = Unix.gettimeofday () in
        let data = List.map
            (function
              | `Real -> (Topic.(Topic (`Real, Time)), `Float (t_end -. t_start))
              | `User -> (Topic.(Topic (`User, Time)), `Float rusage.Lwt_unix.ru_utime)
              | `Sys  -> (Topic.(Topic (`Sys, Time)), `Float rusage.Lwt_unix.ru_stime))
            times in
        Lwt_io.read p#stdout >>= fun stdout ->
        Lwt_io.read p#stderr >>= fun stderr ->
        p#status >>= function
        | Unix.WEXITED return_value ->
            return Result.Execution.{ return_value; stdout; stderr; data; }
        | _ -> fail (Failure (Printf.sprintf "%s has been killed."
                                (String.concat "" cmd)))
      in
      let cmd = "", Array.of_list cmd in
      let t_start = Unix.gettimeofday () in
      Lwt_process.with_process_full ?env cmd (produce_result t_start)

  let run ?env ?(nb_iter=1) cmd times =
    run_n (fun () -> run_once ?env cmd times) nb_iter
end

module Libperf_wrapper = struct
  include Wrapper_common

  let run_once ?env cmd attrs =
    let open Perf in
    let attrs = List.map Perf.Attr.make attrs in
    with_process ?env cmd attrs |> fun {return_value; stdout; stderr; data;} ->
    let data = List.map (fun (k, v) ->
        Topic.(Topic (k, Libperf)), `Int v) data in
    Macroperf.Result.Execution.{ return_value; stdout; stderr; data; }

  let run ?env ?(nb_iter=1) cmd evts =
    run_n (fun () -> wrap (fun () -> run_once ?env cmd evts)) nb_iter
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
      | None -> b.measures
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

    let run_execs { time; gc; libperf; perf; } =
      (match time with
        | [] -> return []
        | _ -> (Time_wrapper.(run ?env:b.env ~nb_iter b.cmd time)))
      >>= fun time_res ->
      (match libperf with
       | [] -> return []
       | _ -> (Libperf_wrapper.(run ?env:b.env ~nb_iter b.cmd libperf)))
      >>= fun libperf_res ->
      (match perf with
       | [] -> return []
       | _ -> (Perf_wrapper.(run ?env:b.env ~nb_iter b.cmd perf)))
      >>= fun perf_res ->
      return @@ time_res @ libperf_res @ perf_res
    in

    run_execs execs >|= fun execs ->
    Result.make ~context_id:"unknown" ~src:b ~execs ()

  let run ?nb_iter ?topics b =
    try
      catch
        (fun () -> run_exn ?nb_iter ?topics b >|= fun r -> Some r)
        (fun _ -> return None)
    with _ -> return None

end
