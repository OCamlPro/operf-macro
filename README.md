# operf-macro

## Introduction

*operf-macro* is a (macro)-benchmarking suite for OCaml. It provides a
framework to define, run, and gather results from macro-benchmarks. A
*macro-benchmark* is an OCaml program whose aim is to measure the
performance of the particular compiler that generated it.

Contrary to *micro-benchmarks* that are OCaml functions of some
parameter(s) representing the size of the problem (size of an array to
iterate on, number of iterations of a loop, etc.), macro-benchmarks do
not have a parameter. The other difference is that, as said above,
they are whole OCaml programs as opposed to functions.

Eventually, the *operf-macro* framework will serve as an unification
layers for presenting results from micro-benchmarks as well. Some
tools are already available, for instance the `injector` program can
import inline micro-benchmark results from the Jane Street *Core*
library into the operf-macro framework.

For now, it is however to stick with the micro-benchmarking tools
already available like
[core_bench](http://github.com/janestreet/core_bench) or
[operf-micro](http://github.com/OCamlPro/operf-micro). Interesting
insight about the *core_bench* library can be found
[here](https://blogs.janestreet.com/core_bench-micro-benchmarking-for-ocaml/).

The other important thing to have in mind from the start is that
*operf-macro* is highly integrated into OPAM:

* macro-benchmarks are OPAM packages
* compilers are OPAM switches

Although there are means to bypass this design rule, it is probably
easier to stick to it. Some pointers will be given in the Usage
section regarding running independent benchmarks. A method will also
be given to transform any OCaml installation into an OPAM switch.

## Installation

You need OPAM version 1.2.

```
$ opam repo add operf-macro git://github.com/vbmithr/opam-bench-repo
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
by "4.01" will be printed on screen.

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

## Advanced usage, extending, etc.

## FAQ

