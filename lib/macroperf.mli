module Topic : sig
  type time = [ `Real | `User | `Sys ]
  type gc = [ `Alloc_major | `Alloc_minor | `Compactions ]

  type _ kind =
    (** Time related *)
    | Time : time kind

    (** GC related *)
    | Gc : gc kind

    (** Use the ocaml-perf binding to perf_event_open(2). *)
    | Libperf : Perf.Attr.kind kind (** Refer to ocaml-perf for numbers *)

    (** Use the perf-stat(1) command (need the perf binary, linux
        only) *)
    | Perf : string kind

  type t =  Topic : 'a * 'a kind -> t
end

module Measure : sig
  type t = [ `Int of int64 | `Float of float | `Error ]
  (** Type of a measure. This is to discriminate between discrete
      events (i.e. cpu cycles), continuous events (i.e. time) and
      errors (the measurement operation failed). *)

  val of_string : string -> t
  (** [of string msr_string] is the measure resulting from the
      cast of [msr_string]. *)
end

module Execution : sig
  type exec = {
    process_status: Unix.process_status;
    stdout: string;
    stderr: string;
    data: (Topic.t * Measure.t) list;
  }
  (** Type representing the successful execution of a benchmark. *)

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  (** Type representing the execution of a benchmark. *)

  val error : exn -> t
  (** [error exn] is `Error Printexc.(to_string exn) *)
end

module Benchmark : sig
  type speed = [`Fast | `Slow | `Slower]

  type t = {
    name: string;
    (** Identifier for a benchmark, should be unique amongst
        benchmarks *)
    descr: string option;
    (** Optional description of the benchmark *)
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
    timeout: int;
    (** Maximum time per execution, in seconds. *)
    topics: Topic.t list;
    (** Set of quantities to measure *)
  }

  val make :
    name:string ->
    ?descr:string ->
    cmd:string list ->
    ?env:string list ->
    ?nb_iter:int ->
    speed:speed ->
    ?timeout:int ->
    topics:Topic.t list ->
    unit ->
    t

  val of_string : string -> t
  val to_string : t -> string
end

module Result : sig
  type t = {
    src: Benchmark.t;
    (** The benchmark used to produce this result *)
    context_id: string;
    (** A unique identifier for the context used to produce the
        benchmark executable: compiler used, build options of this
        compiler, etc. *)
    execs: Execution.t list;
    (** This contain the list of execution results, containing
        measurements plus additional useful information about the
        individual runs if the execution was possible. *)
  }
  (** Type of a result. This can correspond to several runs of the
      same benchmark,if requested measures cannot be performed in one
      go. *)

  val of_string : string -> t
  val to_string : t -> string

  val make :
    src:Benchmark.t ->
    ?context_id:string ->
    execs:[`Ok of Execution.exec | `Timeout | `Exn of exn] list -> unit ->
    t

  val strip : [`Stdout | `Stderr] -> t -> t
  (** [strip t chan is a result where the output of the program
      executions in [chan] have been disabled. *)
end

module Runner : sig
  val run_exn : Benchmark.t -> Result.t
  val run     : Benchmark.t -> Result.t option
end
