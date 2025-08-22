# AsynchSubtypingRegex
This tool implements the asynchronous subtyping algorithm introduced in [1], and in particular allows to execute the experiments described therein.

Instructions:
- Install Scala 3.3.1 and Scala Building Tools (SBT).
- Install Graphviz: https://graphviz.org/
- Download or clone this repository.
- Open terminal on directory AsynchSubtypingRegex and run SBT.
- Type "run file1 file2 dir [-d]" and press enter, where file1 (resp. file2) is the path to the candidate subtype (resp. supertype), dir is the directory where the outputs will be generated. The optional d flag is used for checking dual(file2) < dual(file2)

The generated outputs consists of the graphical representations of the automata under analysis and the corresponding simulation graph. If called with -d, automata and graphs are generated for the duals. 

[1] Laura Bocchi, Andy King, Maurizio Murgia: Asynchronous Session Subtyping by Trace Relaxation. In TACAS 2024. https://link.springer.com/chapter/10.1007/978-3-031-57246-3_12
