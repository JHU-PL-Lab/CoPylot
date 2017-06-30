CoPylot
=======

This directory contains a work-in-progress project for a demand-driven program
analysis for a subset of the Python 2 language.  This document contains
information about compiling and running the analysis tool.

Building
--------

There are three major steps to set up a build environment for CoPylot:

  1. Install OPAM and required libraries.
  2. Download and pin development dependencies.
  3. Build CoPylot itself.

The subsections below walk through these processes.

### OPAM

1. Make sure you have [OCaml][ocaml] and [OPAM][opam] installed on the latest
   version:

        opam init               # necessary for freshly-installed OPAM instances
        eval `opam config env`  # if you do not have OPAM's environment configured
        opam update
        opam upgrade
        opam switch 4.04.1      # or whichever compiler -- this may take a while

2. Install the dependencies:

        opam install oasis batteries menhir ounit ppx_deriving ppx_deriving_yojson ocaml-monadic monadlib

   If your shell hashes binary locations, you may need to clear your hashes now.
   (In bash, `hash -r` does this.)

### Development Dependencies

CoPylot depends upon libraries which tend to develop at the same time as it does
(but which are functionally independent and are designed to be used by other
projects).  To configure this environment, you must first clone the repository
for the dependency and then pin that repository as an OPAM package.

1. Install `jhupllib`:

        git clone https://github.com/JHU-PL-Lab/jhu-pl-lib.git ../jhu-pl-lib
        opam pin add jhupllib ../jhu-pl-lib

2. Install `pds-reachability`:

        git clone https://github.com/JHU-PL-Lab/pds-reachability.git ../pds-reachability
        opam pin add pds-reachability ../pds-reachability

You will need to re-run an appropriate `opam pin` command each time one of these
libraries is changed.

### Building CoPylot

With the above configuration, it is now possible to build CoPylot.

1. Generate configuration:

        oasis setup -setup-update dynamic

2. Configure:

        ./configure

3. Enable tests:

        ocaml setup.ml -configure --enable-tests

4. Build:

        make

5. Run the tests:

        make test

Authors
-------

- Zachary Palmer <zachary.palmer@swarthmore.edu>.
- Tianlu Chen <tchen2@swarthmore.edu>
- Devon Loehr <dloehr1@swarthmore.edu>

## Components

* Our Python 2 parser is based on [ocaml-pythonlib][ocaml-pythonlib].

[ocaml]: https://ocaml.org/
[opam]: https://opam.ocaml.org/
[docker]: https://www.docker.com/
[docker-compose]: https://docs.docker.com/compose/
[ocaml-pythonlib]: https://github.com/m2ym/ocaml-pythonlib
