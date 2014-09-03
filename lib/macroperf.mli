module Topic : sig
  type t =
    (** Time related *)
    | Time_real
    | Time_user
    | Time_sys

    (** GC related *)
    | Allocs_major
    | Allocs_minor
    | Compactions

    (** PERF-STAT(1) related (linux only) *)
    | Perf of string
end

module TSet : Set.S with type elt = Topic.t

module Benchmark : sig
  type speed = [`Fast | `Slow | `Slower]

  type t = private {
    b_name: string;
    (** Identifier for a benchmark, should be unique amongst
        benchmarks *)
    b_descr: string option;
    (** Optional description of the benchmark *)
    b_cmd: string list;
    (** Command line to run the benchmark. The first item of the list
        is the full path of the benchmark executable, or its name if in
        PATH *)
    b_env: string list option;
    (** Optional environment for the benchmark *)
    b_speed: speed;
    (** Use to characterize the execution time of a benchmark *)
    b_measures: TSet.t;
    (** Set of quantities to measure *)
  }

  val make :
    name:string ->
    ?descr:string ->
    cmd:string list ->
    ?env:string list ->
    speed:speed ->
    measures:Topic.t list ->
    unit ->
    t

  val of_string : string -> t
  val to_string : t -> string
end

module Result : sig
  type measure = [ `Int of int | `Float of float | `Error ]

  type t = private {
    res_name: string;
    (** The name of the benchmark *)
    res_cmd: string list;
    (** The command line used to run the benchmark *)
    res_date: Unix.tm option;
    (** The date when the benchmark was run *)
    res_data: (Topic.t * measure) list;
    (** The set of measured quantities during the run *)
  }

  val of_string : string -> t
  val to_string : t -> string

  val make :
    name:string ->
    cmd:string list ->
    ?date:Unix.tm ->
    data:(Topic.t * measure) list ->
    t

  val of_benchmark :
    ?date:Unix.tm ->
    Benchmark.t -> (Topic.t * measure) list -> t
end
