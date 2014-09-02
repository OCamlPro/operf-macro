open Lwt
open Macroperf

module Perf_wrapper = struct

type measure = [ `Int of int | `Float of float | `Error ]
(** A measure can be expressed as an int or a float. *)

let measure_of_string s =
  try `Int (int_of_string s) with _ ->
    try `Float (float_of_string s) with _ ->
      `Error

type result = (string * measure) list
(** A result is a list of perf events and associated measures *)

let run ?env ?(evts=[]) ?nb_iter cmd =
  let nb_iter = match nb_iter with
    | None -> "1"
    | Some nb_iter -> string_of_int nb_iter in
  let perf_array = [|"perf"; "stat"; "-x,"; "-r"; nb_iter |] in
  let perf_array = match evts with
    | [] -> perf_array
    | evts -> Array.append perf_array [|"-e"; String.concat "," evts|] in
  let cmd = (fst cmd, (Array.append perf_array (snd cmd))) in
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
    drain_stderr [] >|= fun res ->
    List.fold_left
      (fun acc l -> match l with
         | [v;"";event] -> (event, measure_of_string v)::acc
         | _ -> acc
      )
      [] res
  in
  Lwt_process.with_process_full ?env cmd produce_result
end

module Runner = struct
  exception Not_implemented
  exception Benchmark_too_long

  let run_exn ?speed ?(nb_iter=1) ?measures b =
    let open Benchmark in
    let measures = match measures with
      | None -> TSet.elements b.b_measures
      | Some ms -> TSet.(inter ms b.b_measures |> elements)
    in
    let runit = match speed with
      | None -> true
      | Some s -> b.b_speed < s
    in
    if not runit then raise_lwt Benchmark_too_long
    else
      Lwt_list.fold_left_s
        (fun acc m -> match m with
           | Topic.Perf evts ->
               Perf_wrapper.run ?env:b.b_env ~evts ~nb_iter b.b_cmd >|= fun r ->
               r @ acc
           | _ -> raise_lwt Not_implemented
        )
        [] measures

  let run ?speed ?(nb_iter=1) ?measures b =
    try Some (run_exn ?speed ~nb_iter ?measures b)
    with _ -> None

end
