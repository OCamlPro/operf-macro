# operf-macro

## Introduction

*operf-macro* is a (macro)-benchmarking (i.e. whole program) suite for
OCaml. It provides a framework to define, run, and measure metrics
from such programs. Those include the elapsed time, the elapsed cycles
and OCaml GC stats. The aim of *macro-benchmarks* is to measure the
performance of the particular compiler that generated it. It can also
be used to compare different versions of a particular program, or
to compare the performance of several programs whose functionality is
equivalent.

Contrary to *micro-benchmarks* that are OCaml functions of some
parameter(s) representing the typical size of the problem (size of an
array to iterate on, number of iterations of a loop, etc.),
macro-benchmarks generally do not have parameters. The other
difference is that, as said above, they are whole OCaml programs as
opposed to functions.

Eventually, the *operf-macro* framework will serve as an unified
layer to present results from micro-benchmarks as well. Some tools
are already available, for instance the `injector` program can import
inline micro-benchmark results from the Jane Street *Core* library
into the *operf-macro* framework.

For now, it is however safer to stick with the micro-benchmarking
tools already available like
[core_bench](http://github.com/janestreet/core_bench) or
[operf-micro](http://github.com/OCamlPro/operf-micro). An interesting
read about the *core_bench* library can be found in the [Jane Street
OCaml
blog.](https://blogs.janestreet.com/core_bench-micro-benchmarking-for-ocaml/).

The other important thing to have in mind from the start is that
*operf-macro* is highly integrated into OPAM:

* macro-benchmarks are OPAM packages
* compilers are OPAM switches

Although there are means to bypass this design principle, it is probably
easier to stick to it. Some pointers will be given in the Usage
section regarding running independent benchmarks. A method will also
be given to transform any OCaml installation into an OPAM switch.

## Installation

You need OPAM version 1.2.

```
$ opam repo add operf-macro git://github.com/OCamlPro/opam-bench-repo
$ (optionally) opam install core async async_smtp core_bench
$ opam install operf-macro all-bench
```

You can install all, some, or no packages listed in the second
lines. They are optional dependencies to some benchmarks.

The last line installs `all-bench`, a meta-package that will always
depend on all the available benchmarks.

## Basic usage

The `operf-macro` benchmark will install an executable named
`macrorun`. This is the single entry-point to the framework and all
functionality derives from it. It is a CLI program, using
[cmdliner](http://erratique.ch/software/cmdliner). You can therefore
easily obtain help on the possible commands directly through it. We
give here only some tips to begin.

### Listing available benchmarks

```
$ macrorun list "4.01*"
```

where `[glob]*` is any number of arguments that will be treated as a
glob (shell) pattern for a complier version. In this case, all
installed benchmarks for available compiler switches whose name starts
by "4.01" will be listed on screen.

### Running benchmarks

```
$ macrorun run
```

This will run all benchmarks installed in the OPAM switch you are
currently in, and gather the results in
`.cache/operf/macro/<benchmark>/`. You can always interrupt the
program during execution: successfully executed benchmarks' results
will be saved. Alternatively, you can use either

```
$ macrorun run [bench_names_glob]*
$ macrorun run --skip [bench_names_glob]*
```

to run only a selection of benchmarks. It will include (resp. exclude)
the selected benchmarks and only those.

### Obtaining results

#### Raw data

`macrorun` stores its results in `~/.cache/operf/macro`. Here you will
find one directory per benchmark, and inside, one `.result` file per
compiler. Inside the file you will find a *s-expression* that is the
serialized version of `macrorun`'s `Result` value. This includes mostly
the individual measurements per execution, such as real time, cycles,
and so on.

#### Summaries

Use:

```
$ macrorun summarize
```

This will print a dump of the database of all `macrorun`'s results, as
an s-expression, on your screen, but before that, it will create a
`.summary` file for each `.result` file (see previous section) found
in `~/.cache/operf/macro`, in the same directory.

#### Result as `.csv` files to feed your favourite plotting program

```
$ macrorun summarize -b csv -t <topic> [-s compiler1,...,compilerN] [benchmark1 ... benchmarkN]
```

This will print a CSV array of requested benchmarks (or all benchmarks
if no benchmarks are specified) for the specified switches (or all
switches if not specified). If you don't specify a topic (`-t`)
option, the output will contain *n* arrays, one per topic, separated
by a newline.

#### Visualizing the results

If you have a recent version of *gnuplot* compiled with its *Qt*
backend installed, you can replace `csv` by `qt` in the example above
(you **need** to specify a topic then). This will launch *gnuplot* in
a window and will display the CSV array as a bar chart. You can use
the `-o` argument to export the gnuplot `.gnu` file.

## Advanced usage, extending, etc.

### Writing benchmarks

#### TL;DR;

Use:

```
$ macrorun perf /path/to/exe arg1 .. argn --batch
```

This will perform a benchmark of the program specified in the
commandline and print the result as an s-expression in stdout. This
s-expression includes an inner s-expression describing the benchmark
source, and this is your benchmark description. Write this in a
`.bench` file.

#### `.bench` file format

Benchmark descriptions must be stored in files with the extension
`.bench`. The format used is an S-expression matching the internal `Benchmark.t` type:

```
  type speed = [`Fast | `Slow | `Slower] with sexp

  type t = {
    name: string;
    descr: string with default("");
    cmd: string list;
    cmd_check: string list with default([]);
    env: string list option with default(None);
    speed: speed with default(`Fast);
    timeout: int with default(600);
    weight: float with default(1.);
    discard: [`Stdout | `Stderr] list with default([]);
    topics: TSet.t with default(TSet.singleton (Topic.(Topic("cycles", Perf))));
  } with sexp
```

- `name` is the name of the benchmark, and must be unique.
- `description` is a free text field.

- `cmd` is a list containing the absolute path of the benchmark
  executable followed by possible arguments. If arguments are paths,
  the *must* be absolute paths.

- `cmd_check` is an optional way to run a program to check if the
  benchmark terminated correctly. The provided string list is the name
  of such a program and its arguments. It will be runned in a shell
  (using `Sys.command`) and in the same directory where the benchmark
  was run, so that the test program can inspect any files produced by
  the benchmark, if needed.

- `env` is an optional list of environment parameters. If empty, the
  environment will be the same as the one in effect when `macrorun`
  was run. It should be of the form `["VAR1=v1";"VAR2=v2"; ...]`
  similar to the `Unix.execve` function.

- `speed` is a indication about the time of execution of a
  benchmark. Some benchmarks run faster than others. `Fast` should be
  used when the execution time is less than 0.1s or so in a typical
  machine, `Slow` when the execution time is of the order of the
  second and `Slower` otherwise.

- `timeout` is the maximum running time in seconds. After the timeout
  expires, a running benchmark is cancelled.

- `weight` is the relative importance of this benchmarks compared to
  others. The default is `1`, for an average importance. This
  parameter is used when computing global performance indices for a
  compiler, including several or all benchmarks.

- `discard` can be specified to indicate to `macrorun` that it should
  not save the output of the program. Usually, the output of the
  program is stored in the `.result` files for ulterior examination.

- `topics` is a list of hints for `macrorun` to know which
  measurements should be done. This field is deprecated and should not
  be used.

## FAQ

### I want `macrorun` to measure GC stats for my program!

Please add at the end of your benchmark:

```
  try
    let fn = Sys.getenv "OCAML_GC_STATS" in
    let oc = open_out fn in
    Gc.print_stat oc;
    close_out oc
  with _ -> ()
```
