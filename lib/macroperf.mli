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
    val micro_dir : string
    val macro_dir : string
    val ls : ?preserve_order:bool -> ?prefix:bool -> string -> string list
    val iter : (string -> unit) -> string -> unit
    val fold : ('a -> string -> 'a) -> 'a -> string -> 'a
    val fold_files : ('a -> string -> 'a) -> 'a -> string -> 'a
    val rm_r : string list -> unit

    val exists : string -> bool
    val kind_exn : string -> Unix.file_kind
    val is_file_exn : string -> bool
    val is_dir_exn : string -> bool
    val kind : string -> Unix.file_kind option
    val is_file : string -> bool option
    val is_dir : string -> bool option
  end

  module File : sig
    val string_of_ic : in_channel -> string
    val lines_of_ic : in_channel -> string list
    val string_of_file : string -> string
    val sexp_of_file_exn : string -> (Sexplib.Type.t -> 'a) -> 'a
    val lines_of_file : string -> string list
    val write_string_to_file: fn:string -> string -> unit
    val with_oc_safe : (out_channel -> 'a) -> string -> 'a
    val with_ic_safe : (in_channel -> 'a) -> string -> 'a
  end

  module Cmd : sig
    val path_of_exe : string -> string
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
  module Time : sig
    type t = Real | User | Sys
  end

  module Gc : sig
    type t =
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

    val of_string_exn : string -> t
    val of_string : string -> t option
  end

  type _ kind =
    (** Time related *)
    | Time : Time.t kind

    (** GC related *)
    | Gc : Gc.t kind

    (** Use the perf-stat(1) command or ocaml-libperf *)
    | Perf : string kind

  type t =  Topic : 'a * 'a kind -> t

  val of_string : string -> t
  val to_string : t -> string
end

module SSet : Set.S with type elt = string
module SMap : Map.S with type key = string
module TSet : Set.S with type elt = Topic.t
module TMap : Map.S with type key = Topic.t

module Benchmark : sig
  type speed = [`Fast | `Slow | `Slower]

  type t = {
    name: string;
    (** Identifier for a benchmark, should be unique amongst
        benchmarks *)
    descr: string;
    (** Description of the benchmark *)
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
    weight: float;
    (** Used to specify the relative importance of this benchmark
        compared to others (default: 1.) *)
    discard: [`Stdout | `Stderr] list;
    (** The runner will discard the output of specified channels in
        the result. *)
    topics: TSet.t;
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
    ?weight:float ->
    ?discard:[`Stdout | `Stderr] list ->
    topics:Topic.t list ->
    unit ->
    t

  (** I/O *)

  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t

  val save_hum : string -> t -> unit
  val output_hum : out_channel -> t -> unit

  (** Filesystem *)
  val find_installed : ?switch:string -> unit -> (string * string) list
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
    bench: Benchmark.t;
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
    bench:Benchmark.t ->
    ?context_id:string ->
    execs:Execution.t list -> unit ->
    t

  val strip : [`Stdout | `Stderr] -> t -> t
  (** [strip t chan] is a result where the output of the program
      executions in [chan] have been disabled. *)

  (** I/O *)

  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t

  val save_hum : string -> t -> unit
  val output_hum : out_channel -> t -> unit
end

module Summary : sig
  module Aggr : sig
    type t = { mean: float;
               stddev: float;
               mini: float;
               maxi: float;
             }

    include Sexpable.S with type t := t

    val create : float -> float -> float -> float -> t

    val compare : t -> t -> int
    val max : t -> t -> t
    val min : t -> t -> t

    val of_measures : Measure.t list -> t
    (** [of_measures weight m] is a t *)

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
    weight: float;
    data: Aggr.t TMap.t;
  }
  (** Content of a "summary file". *)

  include Sexpable.S with type t := t

  val of_result : Result.t -> t

  val normalize : t -> t
  val normalize2 : t -> t -> t
  (** Fails with [Not_found] if the keys of the data TMap.t do not
      match. *)

  (** I/O *)

  val load_from_result : string -> t

  val load_conv : string -> t Sexplib.Sexp.Annotated.conv
  val load_conv_exn : string -> t

  val save_hum : string -> t -> unit
  val output_hum : out_channel -> t -> unit

  (** Operation on directories containing .summary files. *)

  val fold_dir : ('a -> string -> 'a) -> 'a -> string -> 'a
  (** [fold_dir f acc dn] is like [Util.FS.fold f acc dn] except it
      folds only on regular files that have suffix .summary *)

  val summarize_dir : ?update_only:bool -> string -> unit
  (** [summarize_dir ?update_only dn] traverse [dn] and create a
      .summary file for each .result file found. If [?update_only] is
      set, then a .summary file is created only if needed (i.e. there
      is none yet or the existing one is out-of-date. *)
end

module DB : sig
  (** Database of summaries *)

  type 'a t = ('a SMap.t) SMap.t
  (** Indexed by benchmark, context_id, topic. *)

  include Sexpable.S1 with type 'a t := 'a t
  val save_hum : string -> ('a -> Sexplib.Sexp.t) -> 'a t -> unit
  val output_hum : out_channel -> ('a -> Sexplib.Sexp.t) -> 'a t -> unit

  val empty : 'a t

  (** Generic functions *)

  val add : SMap.key -> SMap.key -> 'a -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (SMap.key -> SMap.key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Specific functions *)

  val fold_data : (SMap.key -> SMap.key -> TMap.key -> Summary.Aggr.t -> 'b -> 'b) ->
    Summary.t t -> 'b -> 'b

  val of_dir : ?acc:Summary.t t -> string -> Summary.t t
  (** [of_dir dn] is the db created from the .summary files found from
      the traversal of [dn]. *)

end

module DB2 : sig
  (** Database of summaries, CSV oriented *)

  type 'a t = (('a SMap.t) SMap.t) TMap.t
  (** Indexed by topic, benchmark, context_id *)

  include Sexpable.S1 with type 'a t := 'a t
  val save_hum : string -> ('a -> Sexplib.Sexp.t) -> 'a t -> unit
  val output_hum : out_channel -> ('a -> Sexplib.Sexp.t) -> 'a t -> unit

  val empty : 'a t
  val add : TMap.key -> SMap.key -> SMap.key -> 'a -> 'a t -> 'a t

  val fold : (TMap.key -> SMap.key -> SMap.key -> 'a -> 'b -> 'b) ->
    'a SMap.t SMap.t TMap.t -> 'b -> 'b

  val normalize : ?against:[`Ctx of string | `Biggest] -> Summary.Aggr.t t -> Summary.Aggr.t t
  (** [normalize ~against db] is [db] with aggrs normalized (mean =
      1.). If [~against] is specified, means are normalized against
      what is specified by the polymorphic variant. *)

  val to_csv : ?sep:string -> out_channel -> ?topic:TMap.key -> Summary.Aggr.t t -> int
  (** The integer returned is the number of ctxs_ids found. *)
end


module Runner : sig
  val run_exn : ?use_perf:bool -> ?context_id:string -> interactive:bool -> Benchmark.t -> Result.t
end
