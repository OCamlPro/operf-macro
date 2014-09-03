open Lwt
open Macroperf

module Perf_wrapper = struct

  type event = string

  type measure = [ `Int of int | `Float of float | `Error ]
  (** A measure can be expressed as an int or a float. *)

  let measure_of_string s =
    try `Int (int_of_string s) with _ ->
      try `Float (float_of_string s) with _ ->
        `Error

  type result = (Topic.t * measure) list
  (** A result is a list of perf events and associated measures *)

  let run ?env ?(evt="") ?nb_iter cmd =
    let nb_iter = match nb_iter with
      | None -> "1"
      | Some nb_iter -> string_of_int nb_iter in
    let perf_cmdline = ["perf"; "stat"; "-x,"; "-r"; nb_iter] in
    let perf_cmdline = match evt with
      | "" -> perf_cmdline
      | evt -> perf_cmdline @ ["-e"; evt] in
    let cmd = "", Array.of_list @@ perf_cmdline @ cmd in
    let env = match env with
      | None -> None
      | Some env -> Some (Array.of_list env) in

    let produce_result pfull =
      let rex = Re.(str "," |> compile) in
      let rec drain_stderr acc =
        lwt line =
          try_lwt Lwt_io.read_line pfull#stderr
          with End_of_file -> return ""
        in
        match line with
        | "" -> return acc
        | l -> drain_stderr ((Re_pcre.split ~rex line)::acc)
      in
      pfull#status >>= function
      | Unix.WEXITED rv ->
          drain_stderr [] >|= fun res ->
          rv, List.fold_left
            (fun acc l -> match l with
               | [v;"";event] -> (Topic.Perf event, measure_of_string v)::acc
               | _ -> acc
            )
            [] res
      | _ -> raise_lwt (Failure "perf has been killed")
    in

    Lwt_process.with_process_full ?env cmd produce_result
end

module Runner = struct
  exception Not_implemented

  let run_exn ?(nb_iter=1) ?topics b =
    let open Benchmark in
    let topics = match topics with
      | None -> TSet.elements b.b_measures
      | Some ts -> ts
    in
    (* Transform Perf topics into a combined Perf topic so that
       PERF-STAT(1) will execute them at once. *)
    let perf_evts =
      List.fold_left
        (fun acc topic -> match acc, topic with
           | None, Topic.Perf evt -> Some [evt]
           | Some evts, Topic.Perf evt -> Some (evt::evts)
           | _ -> acc
        ) None topics
    in
    let topics =
      List.filter (function Topic.Perf _ -> false | _ -> true) topics in
    let topics = match perf_evts with
      | None -> topics
      | Some evts -> (Topic.Perf (String.concat "," evts))::topics
      (* At this point, we replaced all individual "Perf _" topics
         with one global "Perf _" containing all the individual
         evts. *)
    in
    Lwt_list.fold_left_s
      (fun acc m -> match m with
         | Topic.Perf evt ->
             (Perf_wrapper.run ?env:b.b_env ~evt ~nb_iter b.b_cmd
              >|= function
                (* Only append the result to the list if the benchmark
                   exited with code 0 *)
              | rv, rs when rv = 0 -> rs @ acc
              | _ -> acc
             )
         | _ -> raise_lwt Not_implemented
      )
      [] topics
    >|= fun data ->
    Result.of_benchmark
      ~date:(Unix.(gettimeofday () |> gmtime)) b data

  let run ?(nb_iter=1) ?topics b =
    try_lwt run_exn ~nb_iter ?topics b >|= fun r -> Some r
    with _ -> return None

end
