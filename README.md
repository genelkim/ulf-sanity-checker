**ULF Sanity Checker**

A quick-and-dirty ULF type analysis and sanity checker to assist in
annotations.  It performs an approximate type analysis of the formula and
identifies combinations of types that are not allowed.

== Dependencies ==
Python 2
Allegro Common Lisp (hopefully we will generalize this soon)
Quicklisp
TTT
genelkim/cl-util
genelkim/ulf-lib


== Install ==

1. Run the install script (TODO: write this...).  This will download the 
appropriate libraries and jars. 

2. Make sure that the local Common Lisp libraries (TTT, cl-util, ulf-lib)
are installed in a location that is discoverable by ASDF and Quicklisp. 

== Versions ==
For details of the systems, please see the README in the respective tagged commits.

* v1.0 was used from Fall 2017 through summer 2018
* v2.0 was introduced in Fall 2018 for the 2018 interannotator agreement experiment, which included some minor changes to the annotation guidelines.


== Running the Checker ==

The simplest way to run the checker is to call local_sanity_checker.sh with a
single argument that is a filepath containing the ULF to check.  The function
`sanity-check` is exported by the package so you can call it directly from your
code.

**Please remember this was quickly written up and minimally tested so there could be errors.  *Always* think about whether there is in fact an error before making changes.**

Paste 1 formula into a file then run:
./local_sanity_check.sh [filename]

Sanity check performs the following steps:
Preprocessing:
  1. Remove curly brackets that are not escaped
  2. preprocess quotes so they can be read into Lisp
  3. Filter commas, semicolons, integrated quotes
  4. Extract sentence-leve operators : not, (adv-e ..), *.adv-e
  Return ([filtered sentence], [list of sentence-level ops])

Sanity check (for filtered sentence and each sentence-level op):
  1. Recursively guess type for formula constituents.
  2. Look for obvious errors (e.g. wrong number of arguments, tense mismatch, etc.)

It will print the formula after preprocessing and the formula with the predicted types prefixed to each constituent recursively (this type marking is a bit hard to read).
Then it will display segments of the annotation that it predicts have errors (they look like this):

Ann segment: ((PRES BE.V) (SOMEWHAT.ADV-A ((PAST EXPLAIN.V) (ADV-A (AT.P (THE.D END.N))))))
Predicted constituent types ((list of types) -- constituent)
  ((PRED TENSED-VERB) -- (PRES BE.V))
  ((PRED TENSED-VERB) -- (SOMEWHAT.ADV-A ((PAST EXPLAIN.V) (ADV-A (AT.P (THE.D END.N))))))
Possibly failed conditions:
  "Each embedded sentence should only have 1 tense operator."

First it prints the segment.  Then it lists the constituents with their predicted types. 
Then is gives a general description of the expected conditions given the types.

Few thinks to look for:
- Errors may appear because you incorrectly annotated a sentence-level operator, which gets lifted out.
- Since UNKNOWN types are propagated, it's best to first look for where an UNKNOWN type is introduced and check if there's a mistake in the formula or if the sanity checker is wrong.

