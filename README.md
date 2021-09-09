# psilisp

Iterations of an experimental lisp dialect I've been denoting on scratch paper
for a long time which has finally moved to real code.

This project is a "whenever I feel like it" project. So maybe it has days,
months, or years, where it doesn't get touched.

The code is not production, may change wildly, and is not stable.

## C0 

This is most of the "An Incremental Scheme Compiler" by Abdulaziz Ghuloum.
The extended tutorial is here also writte by Ghuloum.

I also picked up additional information from:
;;;; http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
;;;; https://github.com/namin/inc/blob/master/docs/tutorial.pdf?raw=true

The tutorial cuts off at a bad time in the explanation and I didn't
want to continue it in the current pedagogical direction it was taking.

This is because I want a more traditional representation of the AST and
to work out how to do optimizations, represent debugging information,
and remove the need for a C runtime in its entirety. 

So I abandoned it.

## C1

This is a brand new compiler written in the style of an introductory 
compiler class in college. I still use the host language forms as the
input language for now.

Some of my goals for this compiler are:
	- Exploring closure representations: Flat vs Live Variable Analysis
	- Multiple IR representations: AST, DAG, Linear
	- Embedding of X86/X86_64 and ARM32/ARM64 *directly* into the language.
	- Optimization passes.
	- Tree matching for multiple backend code generation.
	- No C runtime AT ALL.
	- Bootstrapping the image of the language with itself.
	- Type theory between static and dynamic types.
	- Type inference.
	- Debugging annotations.
	- Public API to the internal compilation passes and data structures.
	- Compiler "policy" description and enforcement for code regions.
	- Parallel execution both threads and processes.
	- etc, etc, etc.


