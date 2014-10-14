module Sexpable : sig
  module type S = sig
    type t with sexp
  end
  module type S1 = sig
    type 'a t with sexp
  end
  module type S2 = sig
    type ('a, 'b) t with sexp
  end
  module type S3 = sig
    type ('a, 'b, 'c) t with sexp
  end
end

module Util : sig
  module FS : sig
    val (/) : string -> string -> string
    val home : string
    val cache_dir : string
    val ls : ?preserve_order:bool -> ?prefix:bool -> string -> string list
    val iter : (string -> unit) -> string -> unit
    val fold : ('a -> string -> 'a) -> 'a -> string -> 'a
    val rm_r : string -> unit
  end

  module File : sig
    val string_of_ic : in_channel -> string
    val lines_of_ic : in_channel -> string list
    val string_of_file : string -> string
    val sexp_of_file_exn : string -> (Sexplib.Type.t -> 'a) -> 'a
    val lines_of_file : string -> string list
    val write_string_to_file: fn:string -> string -> unit
    val with_oc_safe : (out_channel -> 'a) -> string -> 'a
  end

  module Cmd : sig
    val stdout_of_cmd : string -> Unix.process_status * string
  end

  module Opam : sig
    val (/) : string -> string -> string
    (** Alias to Filename.concat *)

    val home : string
    (** $HOME *)

    val root : string
    (** [root] is either $OPAMROOT or $HOME/.opam if unset *)

    val switch : string
    (** [switch] is the name of the current OPAM switch. *)

    val share : ?switch:string -> unit -> string
    (** [share] is the name of the share folder under the current
        OPAM switch. *)
  end
end

module Topic : sig
  type time = Real | User | Sys
  type gc =
    Minor_words
    | Promoted_words
    | Major_words
    | Minor_collections
    | Major_collections
    | Heap_words
    | Heap_chunks
    | Top_heap_words
    | Live_words
    | Live_blocks
    | Free_words
    | Free_blocks
    | Largest_free
    | Fragments
    | Compactions

  val gc_of_string_exn : string -> gc
  val gc_of_string : string -> gc option

  type _ kind =
    (** Time related *)
    | Time : time kind

    (** GC related *)
    | Gc : gc kind

    (** Use the ocaml-perf binding to perf_event_open(2). *)
    | Libperf : Perf.Attr.Kind.t kind (** Refer to ocaml-perf for numbers *)

    (** Use the perf-stat(1) command (need the perf binary, linux
        only) *)
    | Perf : string kind

  type t =  Topic : 'a * 'a kind -> t

  val of_string : string -> t
  val to_string : t -> string
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
    cmd_check: string list;
    (** Command line of the check program. It is used to check if the
        benchmark has executed correctly. *)
    env: string list option;
    (** Optional environment for the benchmark *)
    speed: speed;
    (** Use to characterize the execution time of a benchmark *)
    timeout: int;
    (** Maximum time per execution, in seconds. *)
    topics: Topic.t list;
    (** Set of quantities to measure *)
  }

  include Sexpable.S with type t := t

  val make :
    name:string ->
    ?descr:string ->
    cmd:string list ->
    ?cmd_check:string list ->
    ?env:string list ->
    speed:speed ->
    ?timeout:int ->
    topics:Topic.t list ->
    unit ->
    t
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

module SSet : Set.S with type elt = string
module SMap : Map.S with type key = string
module TMap : Map.S with type key = Topic.t

module Execution : sig
  type exec = {
    process_status: Unix.process_status;
    stdout: string;
    stderr: string;
    data: Measure.t TMap.t;
    checked: bool option;
  }
  (** Type representing the successful execution of a benchmark. *)

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  (** Type representing the execution of a benchmark. *)

  val error : exn -> t
  (** [error exn] is `Error Printexc.(to_string exn) *)

  val duration : t -> Int64.t
  (** [duration e] is the duration of [e] in nanoseconds. *)

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


  include Sexpable.S with type t := t

  val make :
    src:Benchmark.t ->
    ?context_id:string ->
    execs:Execution.t list -> unit ->
    t

  val strip : [`Stdout | `Stderr] -> t -> t
  (** [strip t chan is a result where the output of the program
      executions in [chan] have been disabled. *)
end

module Summary : sig
  module Aggr : sig
    type t = { mean: float; stddev: float; mini: float; maxi: float; }

    include Sexpable.S with type t := t

    val of_measures : Measure.t list -> t

    val normalize : t -> t
    (** [normalize a] is [a] where all the fields are divided by
        [a.mean]. *)

    val normalize2 : t -> t -> t
    (** [normalize2 a b] is [b] where all the fields are divided by
        [b.mean]. *)
  end

  type t = {
    name: string;
    context_id: string;
    data: Aggr.t TMap.t;
  }
  (** Content of a "summary file". *)

  include Sexpable.S with type t := t

  val of_result : Result.t -> t
end

module DB : sig
  (** Database of summaries *)

  type 'a t = (('a TMap.t) SMap.t) SMap.t
  (** Indexed by benchmark, context_id, topic. *)

  include Sexpable.S1 with type 'a t := 'a t

  val empty : 'a t

  val add : SMap.key -> SMap.key -> TMap.key -> 'a -> 'a t -> 'a t
  val add_tmap : SMap.key -> SMap.key -> 'a TMap.t -> 'a t -> 'a t

  val fold : (SMap.key -> SMap.key -> TMap.key -> 'a -> 'b -> 'b) ->
    'a TMap.t SMap.t SMap.t -> 'b -> 'b
end

module DB2 : sig
  (** Database of summaries, CSV oriented *)

  type 'a t = (('a SMap.t) SMap.t) TMap.t
  (** Indexed by topic, benchmark, context_id *)

  include Sexpable.S1 with type 'a t := 'a t

  val empty : 'a t
  val add : TMap.key -> SMap.key -> SMap.key -> 'a -> 'a t -> 'a t

  val fold : (TMap.key -> SMap.key -> SMap.key -> 'a -> 'b -> 'b) ->
    'a SMap.t SMap.t TMap.t -> 'b -> 'b

  val normalize : ?context_id:string -> Summary.Aggr.t t -> Summary.Aggr.t t

  val context_ids : 'a t -> SSet.t

  val to_csv : ?sep:string -> out_channel -> ?topic:TMap.key -> Summary.Aggr.t t -> unit
end


module Runner : sig
  val run_exn : Benchmark.t -> Result.t
  val run     : Benchmark.t -> Result.t option
end
