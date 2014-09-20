open Sexplib.Std

module Topic = struct
  type time = [ `Real | `User | `Sys ] with sexp
  type gc = [ `Alloc_major | `Alloc_minor | `Compactions ] with sexp

  type t =
    (* Time related *)
    | Time of time

    (* GC related *)
    | Gc of gc

    (* PERF-STAT(1) related (linux only) *)
    | Perf of string with sexp

  let compare = Pervasives.compare
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
    measures: Topic.t list;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~name ?descr ~cmd ?env ?(nb_iter=1)  ~speed ~measures () =
    { name; descr; cmd; env; nb_iter; speed; measures; }
end

module Result = struct
  module Measure = struct
    type t = [ `Int of int | `Float of float | `Error ] with sexp
    let of_string s =
      try `Int (int_of_string s) with _ ->
        try `Float (float_of_string s) with _ ->
          `Error
  end

  module Execution = struct
    type t = {
      return_value: int;
      stdout: string;
      stderr: string;
      data: (Topic.t * Measure.t) list;
    } with sexp
    (** Type representing the execution of a benchmark. *)

    let make ~return_value ~stdout ~stderr ~data =
      { return_value; stdout; stderr; data; }
  end

  type t = {
    src: Benchmark.t;
    context_id: string;
    execs: Execution.t list;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~src ?(context_id="") ~execs () =
    { src; context_id; execs; }
end
