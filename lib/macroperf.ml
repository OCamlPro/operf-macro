open Sexplib.Std

module Topic = struct
  type time = [ `Real | `User | `Sys ] with sexp
  type gc = [ `Alloc_major | `Alloc_minor | `Compactions ] with sexp

  type _ kind =
    (* Time related *)
    | Time : time kind

    (* GC related *)
    | Gc : gc kind

    (* Use the ocaml-perf binding to perf_event_open(2). *)
    | Libperf : Perf.Attr.kind kind

    (* Use the perf-stat(1) command (need the perf binary, linux
       only) *)
    | Perf : string kind

  type t = Topic : 'a * 'a kind -> t

  let sexp_of_t t =
    let open Sexplib.Sexp in
    match t with
    | Topic (time, Time) -> List [Atom "Time"; sexp_of_time time]
    | Topic (gc, Gc) -> List [Atom "Gc"; sexp_of_gc gc]
    | Topic (libperf, Libperf) -> List [Atom "Libperf"; Perf.Attr.sexp_of_kind libperf]
    | Topic (perf, Perf) -> List [Atom "Perf"; sexp_of_string perf]

  let t_of_sexp s =
    let open Sexplib.Sexp in
    match s with
    | List [Atom "Time"; t] -> Topic (time_of_sexp t, Time)
    | List [Atom "Gc"; t] -> Topic (gc_of_sexp t, Gc)
    | List [Atom "Libperf"; t] -> Topic (Perf.Attr.kind_of_sexp t, Libperf)
    | List [Atom "Perf"; t] -> Topic (string_of_sexp t, Perf)
    | _ -> invalid_arg "t_of_sexp"

  let compare = Pervasives.compare
end

module Measure = struct
  type t = [ `Int of int64 | `Float of float | `Error ] with sexp
  let of_string s =
    try `Int (Int64.of_string s) with _ ->
      try `Float (float_of_string s) with _ ->
        `Error
end

module Execution = struct
  type process_status = Unix.process_status

  let sexp_of_process_status ps =
    let open Unix in
    let open Sexplib.Sexp in
    match ps with
    | WEXITED n -> List [Atom "WEXITED"; sexp_of_int n]
    | WSIGNALED n -> List [Atom "WSIGNALED"; sexp_of_int n]
    | WSTOPPED n -> List [Atom "WSTOPPED"; sexp_of_int n]

  let process_status_of_sexp s =
    let open Unix in
    let open Sexplib.Sexp in
    match s with
    | List [Atom "WEXITED"; n] -> WEXITED (int_of_sexp n)
    | List [Atom "WSIGNALED"; n] -> WSIGNALED (int_of_sexp n)
    | List [Atom "WSTOPPED"; n] -> WSTOPPED (int_of_sexp n)
    | _ -> invalid_arg "process_status_of_sexp"

  type exec = {
    process_status: process_status;
    stdout: string;
    stderr: string;
    data: (Topic.t * Measure.t) list;
  } with sexp

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  with sexp
  (** Type representing the execution of a benchmark. *)

  let error exn = `Error Printexc.(to_string exn)

  let strip chan t = match t, chan with
    | `Timeout, _
    | `Error _, _ -> t
    | `Ok e, `Stdout -> `Ok { e with stdout="" }
    | `Ok e, `Stderr -> `Ok { e with stderr="" }
end

module Benchmark = struct

  type speed = [`Fast | `Slow | `Slower] with sexp

  type t = {
    name: string;
    descr: string option;
    cmd: string list;
    env: string list option;
    nb_iter: int;
    speed: speed;
    topics: Topic.t list;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~name ?descr ~cmd ?env ?(nb_iter=1)  ~speed ~topics () =
    { name; descr; cmd; env; nb_iter; speed; topics; }
end

module Result = struct
  type t = {
    src: Benchmark.t;
    context_id: string;
    execs: Execution.t list;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~src ?(context_id="") ~execs () =
    let execs = List.map (function
        | `Ok r -> `Ok r
        | `Timeout -> `Timeout
        | `Exn exn ->`Error (Printexc.to_string exn)
      ) execs in
    { src; context_id; execs; }

  let strip chan t = match chan with
    | `Stdout ->
        { t with execs = List.map (Execution.strip `Stdout) t.execs }
    | `Stderr ->
        { t with execs = List.map (Execution.strip `Stderr) t.execs }
end
