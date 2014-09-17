open Lwt
open Macroperf

let section = Lwt_log.Section.make "macroperf_lwt"

module Perf_wrapper = struct

  type event = string

  type measure = [ `Int of int | `Float of float | `Error ]
  (** A measure can be expressed as an int or a float. *)

  let measure_of_string s =
    try `Int (int_of_string s) with _ ->
      try `Float (float_of_string s) with _ ->
        `Error

  type result = {
    return_value: int;
    stdout: string;
    res: (Topic.t * measure) list
  }
  (** A result is a list of perf events and associated measures *)

  let run ?env ?(evt="") ?(nb_iter=1) cmd =
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
    let perf_cmdline = ["perf"; "stat"; "-x,"; "-r"; string_of_int nb_iter] in
    let perf_cmdline = match evt with
      | "" -> perf_cmdline
      | evt -> perf_cmdline @ ["-e"; evt] in
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
          {
            return_value;
            stdout;
            res=(List.fold_left
                (fun acc l -> match l with
                   | [v;"";event; ] -> (Topic.Perf event, measure_of_string v)::acc
                   | [v;"";event; _] -> (Topic.Perf event, measure_of_string v)::acc
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
end

module Runner = struct
  exception Not_implemented

  let run_exn ?nb_iter ?topics b =
    let nb_iter = match nb_iter with
      | None -> b.Benchmark.nb_iter
      | Some nb_iter -> nb_iter in
    let open Benchmark in
    let topics = match topics with
      | None -> TSet.elements b.measures
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
             let open Perf_wrapper in
             (run ?env:b.env ~evt ~nb_iter b.cmd
              >|= function
                (* Only append the result to the list if the benchmark
                   exited with code 0 *)
              | { return_value; stdout; res } when return_value = 0 ->
                  res @ acc
              | _ -> acc
             )
         | _ -> raise_lwt Not_implemented
      )
      [] topics
    >|= fun data ->
    Result.make
      ~date:(Unix.gettimeofday ()) ~src:b ~data ()

  let run ?nb_iter ?topics b =
    try_lwt run_exn ?nb_iter ?topics b >|= fun r -> Some r
    with _ -> return None

end
