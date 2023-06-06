# AsynchSubtypingRegex
This tool implements the asynchronous subtyping algorithm introduced in [1], and in particular allows to execute the experiments described therein.

Instructions:
- Install Scala 3.2.2 and Scala Building Tools (SBT).
- Download or clone this repository.
- Open terminal on directory AsynchSubtypingRegex and run SBT.
- Type "run" and press enter. This will execute the benchmarks.

The output consists of:
- A log file called "positiveLog.txt" which contains the results of the positive test.
- For each positive test, a graphical representations of the automata under analysis and the corresponding simulation graph (in the Graphs folder). If the test required the dual construction, automata and graphs are generated for the duals as well. 
- A log file called "negativeLog.txt" which contains the results of the negative test.
- Five log files "benchmarkPositiveLog1.txt",...,"benchmarkPositiveLog5.txt" which contain information on termination times for the positive tests. It must be remarked that termination times are misured with the generation of automata and graphs disabled. 

[1] Laura Bocchi, Andy King, Maurizio Murgia: Asynchronous Session Subtyping by Trace Relaxation.
