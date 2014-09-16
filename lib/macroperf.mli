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
    name: string;
    (** Identifier for a benchmark, should be unique amongst
        benchmarks *)
    descr: string option;
    (** Optional description of the benchmark *)
    deps: string list;
    (** List of OPAM packages on which this benchmark depends *)
    cmd: string list;
    (** Command line to run the benchmark. The first item of the list
        is the full path of the benchmark executable, or its name if in
        PATH *)
    env: string list option;
    (** Optional environment for the benchmark *)
    nb_iter: int;
    (** Number of iterations *)
    speed: speed;
    (** Use to characterize the execution time of a benchmark *)
    measures: TSet.t;
    (** Set of quantities to measure *)
  }

  val make :
    name:string ->
    ?descr:string ->
    ?deps:string list ->
    cmd:string list ->
    ?env:string list ->
    ?nb_iter:int ->
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
    src: Benchmark.t;
    (** The benchmark used to produce this result *)
    date: Unix.tm option;
    (** The date when the benchmark was run *)
    switch: string;
    (** The version of the compiler used to compile the benchmark
        program *)
    data: (Topic.t * measure) list;
    (** The set of measured quantities during the run *)
  }

  val of_string : string -> t
  val to_string : t -> string

  val make :
    src:Benchmark.t ->
    ?date:Unix.tm ->
    ?switch:string ->
    data:(Topic.t * measure) list -> unit ->
    t
end
