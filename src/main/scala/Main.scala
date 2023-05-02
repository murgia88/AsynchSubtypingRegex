import scala.util.parsing.combinator.*
import java.io.File

@main def hello() = {
TestNegative
TestPositive
BenchmarkPositive
}
 
def k = 1
 
def Subtype(M1:Automaton, M2:Automaton):returnType = {
val sg = new SimGraph(M1,M2);
sg.buildSimGraph(k,50)._2
}

def SubtypeAndOutputGraph(M1:Automaton, M2:Automaton,dir:String):returnType = {
val sg = new SimGraph(M1,M2);
val ret = sg.buildSimGraph(k,50);
//val st = SimTree(M1,M2).buildSimTree(0,Some(Set(M2.treeOfState(0))),50);
val path = java.nio.file.Paths.get(dir + "/");
if !java.nio.file.Files.exists(path) then {java.nio.file.Files.createDirectories(path)};
M1.toDot(dir + "/M1.dot",dir + "/M1.svg");
M2.toDot(dir + "/M2.dot",dir + "/M2.svg");
sg.toDot(dir + "/graph.dot",dir + "/graph.svg");
//st.toDot(dir + "/tree.dot",dir + "/tree.svg");
ret._2
}
 
def TestList(list:List[(String,String)],logFileName:String,expectedRes:Boolean,dualFlag:Boolean,outputGraph:Boolean,timeAndInfo:Boolean) = {
val p = new SessionTypeParser()
var total = 0
var passed = 0
var failed = List[(String,String)]()
val logFile = new java.io.PrintWriter(logFileName)
var timeOld = System.currentTimeMillis()
for (file1,file2) <- list do {
  if (timeAndInfo && total>0) then {
    val currentTime = System.currentTimeMillis()
    logFile.println("Execution time: " + (currentTime - timeOld))
    timeOld = currentTime
  }
  total = total + 1
  var type1 = scala.io.Source.fromFile(new File("testsJulien/"+file1)).mkString.mkString.replaceAll("\\s","");
  var type2 = scala.io.Source.fromFile(new File("testsJulien/"+file2)).mkString.mkString.replaceAll("\\s","");
  logFile.println("Checking: " + file1 + " < " + file2)
  logFile.flush()
  p.parse(p.sessionType, type1) match
  	case p.Success(result, next) => {
  		println(file1 + " parsed")
  		if result.isWellFormed() then{
  			println(file1 + " is well formed")
  			val M1 = result.toAutomaton()
  			p.parse(p.sessionType, type2) match
  				case p.Success(result, next) => {
  					println(file2 + " parsed")
  					if result.isWellFormed() then{
  						println(file2 + " is well formed")
  						val M2 = result.toAutomaton()
  						//logFile.println(res)
  						val dir = ("Graphs/"+file1+file2).replaceAll("""[.\s]+""","")
  						val res = if outputGraph then SubtypeAndOutputGraph(M1,M2,dir) else Subtype(M1,M2)
  						logFile.println(res)
  						if res == returnType.Maybe && !dualFlag then{
  						  logFile.println(file1 + " " + file2 +" FAILED")
  						  failed = (file1,file2)::failed
  						}
  						if res == returnType.True && !expectedRes then{
  						  println("ooops")
  						  logFile.println(file1 + " " + file2 +" FAILED")
  						  failed = (file1,file2)::failed
  						}else if res == returnType.True && expectedRes then{
  						  logFile.println(file1 + " " + file2 +" PASSED")
  						  passed = passed + 1;					
  						}else if !dualFlag then{
  						  if expectedRes then{
  						    logFile.println(file1 + " " + file2 +" FAILED")
  						    failed = (file1,file2)::failed
  						  }else{
  						    logFile.println(file1 + " " + file2 +" PASSED")
  						    logFile.println("BUILDING SIM-TREE")
  						    val st = SimTree(M1,M2).buildSimTree(0,Some(Set(M2.treeOfState(0))),25);
  						    if (SimTree(M1,M2).hasErrors(st)) logFile.println("SUBTYPING DISPROVED")
  						    else logFile.println("COULD NOT DISPROVE SUBTYPING")
  						    passed = passed + 1;
  						  }
  						}else{
  						  logFile.println("TRYING DUALS")
  						  //scala.io.StdIn.readLine()
  						  val D1 = M1.dual()
  						  val D2 = M2.dual()
  						  val dir = ("Graphs/"+file1+file2+"Dual").replaceAll("""[.\s]+""","")
  						  val res = if outputGraph then SubtypeAndOutputGraph(D2,D1,dir) else Subtype(D2,D1)
  						  if (res == returnType.fromBool(expectedRes)) then{
  						    logFile.println(file1 + " " + file2 +" PASSED")
  						    passed = passed + 1;
  						  }
  						  else {
  						    logFile.println(file1 + " " + file2 +" FAILED")
  						    failed = (file1,file2)::failed
  						  }
  						}
  					} 
  					else {
  					  logFile.println(file2 + " is not well formed")
  					  failed = (file1,file2)::failed
  					}
  				}
  				case _ => {
  				  logFile.println(file2 + " not parsed.")
  				  failed = (file1,file2)::failed
  				}
  		} 
  		else {
  		  logFile.println(file1 + " is not well formed")
  		  failed = (file1,file2)::failed
  		}
  	}
  	case _ => {
  	  logFile.println(file1 + " not parsed.")
  	  failed = (file1,file2)::failed
  	}
}
if (timeAndInfo) then {
    val currentTime = System.currentTimeMillis()
    logFile.println("Execution time: " + (currentTime - timeOld))
}
logFile.println("Total = " + total)
logFile.println("Passed = " + passed)
logFile.println("Failed:")
logFile.println(failed)
logFile.flush()
} 
 
def TestPositive = {
TestList(PositiveList,"positiveLog.txt",true,true,true,false)
}

def TestNegative = {
TestList(NegativeList,"negativeLog.txt",false,true,false,false)
}

def BenchmarkPositive = {
TestList(PositiveList,"benchmarkPositiveLog1.txt",true,true,false,true)
TestList(PositiveList,"benchmarkPositiveLog2.txt",true,true,false,true)
TestList(PositiveList,"benchmarkPositiveLog3.txt",true,true,false,true)
TestList(PositiveList,"benchmarkPositiveLog4.txt",true,true,false,true)
TestList(PositiveList,"benchmarkPositiveLog5.txt",true,true,false,true)
}

def PositiveList = List(("alceste-T.txt","alceste-U.txt"),
("mixed1.txt","mixed2.txt"),
("alcestewiki1.txt","alcestewiki2.txt"),
("async1.txt","async2.txt"),
("async2.txt","async3.txt"),
("async1.txt","async3.txt"),
("async1choice.txt","async2choice.txt"),
("doublerec.txt","doublerec2.txt"),
("doublerec.txt","singlerex.txt"),
("singlerex.txt","doublerec2.txt"),
("singlerex.txt","doublerec3.txt"),
("singlerex.txt","doublerec4.txt"),
("singlerex.txt","doublerec5.txt"),
("singlerex.txt","doublerec6.txt"),
("gchat1.txt","gchat2.txt"),
("heavy1.txt","heavy2.txt"),
("heavy2.txt","heavy1.txt"),
("lessinternal.txt","moreinternal.txt"),
("moreexternal.txt","lessexternal.txt"),
("norec.txt","rec.txt"),
("testa.txt","testb.txt"),
("test1.txt","test2.txt"),
("test2.txt","test3.txt"),
("test1.txt","test3.txt"),
("wikiv2-1.txt","wikiv2-2.txt"),
("wikiv2-1.txt","wikiv2-3.txt"),
("singlerexrev.txt","singlerex.txt"),
("infinite1seq.txt","infinite2seq.txt"),
("singlerexunfold2.txt","doublerec3.txt"),
("singlerexunfold.txt","doublerec2.txt"),
("singlerexunfold.txt","doublerec3.txt"),
("singlerexunfold.txt","doublerec4.txt"),
("singlerexunfold.txt","doublerec6.txt"),
("baba.txt","bbaa.txt"),
("alternation1.txt","alternation2-ok.txt"),
("goal1.txt","goal2.txt"),
("goal2.txt","goal3.txt"),
("goal1.txt","goal3.txt"),
("goal1-ko.txt","goal2-ko.txt"),
("goal1-ko.txt","goal3-ko.txt"),
("goal2-ko.txt","goal3-ko.txt"),
("goal2rcv1.txt","goal2rcv2.txt"),
("goal2rcv1.txt","goal2rcv2.txt"),
("goal2rcv1-ko.txt","goal2rcv2-ko.txt"),
("multibranch1.txt","multibranch2.txt"),
("recaab1.txt","recaab2.txt"),
("sendend1-ok.txt","sendend2.txt"),
("weakExtChoice1.txt","weakExtChoice2.txt"),
("pattern1.txt","pattern2.txt"),
("sndrcvorder2.txt","sndrcvorder1.txt"),
("reflsnd1.txt","reflsnd1.txt"),
("reflrcv2.txt","reflrcv2.txt"),
("syncloop1.txt","syncloop2.txt"),
("infinite1.txt","infinite2.txt"),
("infinitea.txt","infiniteb.txt"),
("infinite1extra.txt","infinite2extra.txt"),
("gchat1-nonbranch.txt","gchatrev1-nobranch.txt"),
("june1.txt","june2.txt"),
("offset1.txt","offset2.txt"),
("innerloop1-dual.txt","innerloop2-dual.txt"),
("snewex1.txt","snewex2.txt"),
("recinbranch2.txt","recinbranch1.txt"),
("recctxt1.txt","recctxt2.txt"),
("alternation1-rec.txt","alternation2-ok-rec.txt"),
("patternthenloop1.txt","patternthenloop2.txt"),
("forall1.txt","forall2.txt"),
("july1.txt","july2.txt"),
("september1.txt","september2.txt"),
("decidablilityex1.txt","decidablilityex2.txt"),
("smallinnerloop1.txt","innerloop2.txt"),
("innerloop1.txt","innerloop2.txt"),
("twinstar.txt","ex2.txt"),//TODO Slow!!! PASSED!!!
("mario-b.txt","ex2.txt"),//TODO Slow!!! PASSED!!!
("expensive1.txt","expensive2.txt"),
("ctxta1.txt","ctxta2.txt"),
("ctxtb1.txt","ctxtb2.txt"),
("badseq1.txt","badseq2.txt"),
("march3testa1.txt","march3testa2.txt"),
("march3testa1.txt","march3testb2.txt"), //FAILED
("14may2.txt","14may1.txt"),
("aaaaaab1.txt","aaaaaab2.txt"),
("ex1-ok-loop.txt","ex2-ok-loop.txt"))//TODO Slow!!! PASSED!!!
 
def NegativeList = List(("swap1.txt","swapko.txt"),
("swapko.txt","swap1.txt"),
("swap2.txt","swapko.txt"),
("swapko.txt","swap2.txt"),
("swapko2.txt","swap12.txt"),
("swap12.txt","swapko2.txt"),
("alceste-T.txt","alceste-V.txt"),
("alcestewiki2.txt","alcestewiki1.txt"),
("async2.txt","async1.txt"),
("async3.txt","async2.txt"),
("async3.txt","async1.txt"),
("async2choice.txt","async1choice.txt"),
("doublerec2.txt","doublerec.txt"),
("singlerex.txt","doublerec.txt"),
("doublerec2.txt","singlerex.txt"),
("doublerec3.txt","singlerex.txt"),
("gchat2.txt","gchat1.txt"),
("rec.txt","norec.txt"),
("testb.txt","testa.txt"),
("test2.txt","test1.txt"),
("test3.txt","test2.txt"),
("test3.txt","test1.txt"),
("infinite1.txt","bad.txt"),
("simp.txt","bad.txt"),
("bad.txt","simp.txt"),
("infinite1.txt","infinite1seq.txt"),
("infinite1.txt","infinite2seq.txt"),
("infinite2.txt","infinite1seq.txt"),
("infinite2.txt","infinite2seq.txt"),
("infinite2seq.txt","infinite2.txt"),
("infinite1seq.txt","infinite2.txt"),
("infinite2seq.txt","infinite1.txt"),
("infinite1seq.txt","infinite1.txt"),
("bbaa.txt","baba.txt"),
("gchatrev1.txt","gchat2.txt"),
("gchatrev1.txt","gchat1.txt"),
("alternation1.txt","alternation2.txt"),
("alternation1rcv.txt","alternation2rcv.txt"),
("mixed2.txt","mixed1.txt"),
("goal1.txt","goal1-ko.txt"),
("goal1.txt","goal2-ko.txt"),
("goal2.txt","goal3-ko.txt"),
("goal1.txt","goal3-ko.txt"),
("goal1-ko.txt","goal2.txt"),
("goal1-ko.txt","goal3.txt"),
("goal2rcv1-ko.txt","goal2rcv2.txt"),
("goal2rcv1.txt","goal2rcv2-ko.txt"),
("multibranch2.txt","multibranch1.txt"),
("sendend1.txt","sendend2.txt"),
("weakExtChoice2.txt","weakExtChoice1.txt"),
("nestedrec1-ko.txt","nestedrec2.txt"),
("mess1.txt","mess2.txt"),
("messa1.txt","messa2.txt"),
("sndrcvorder1.txt","sndrcvorder2.txt"),
("stackabb1.txt","stackba2.txt"),
("stackba2.txt","stackabb1.txt"),
("lgg1.txt","lgg2.txt"),
("lgg2.txt","lgg1.txt"),
("zzabb1.txt","zzba1.txt"),
("zzabb2.txt","zzba2.txt"),
("loop1.txt","loop2.txt"),
("loop2.txt","loop1.txt"),
("moreinternal.txt","lessinternal.txt"),
("lessexternal.txt","moreexternal.txt"),
("alceste-V.txt","alceste-U.txt"),
("alceste-U-dual.txt","alceste-V-dual.txt"),
("gchat2.txt","gchatrev1.txt"),
("recaab2.txt","recaab1.txt"),
("recctxt1-ko.txt","recctxt2.txt"),
("alternation1-rec.txt","alternation2-rec.txt"),
("nestedrec1.txt","nestedrec2-ko.txt"),
("minirec1.txt","minirec2.txt"),
("blacktriinside1.txt","blacktriinside2.txt"),
("blacktriinside2.txt","blacktriinside1.txt"),
("pattern1ko.txt","pattern2.txt"),
("patternako.txt","patternb.txt"),
("alceste-U.txt","alceste-V.txt"),
("nestedrec1.txt","nestedrec2.txt"),
("alceste-V-dual.txt","alceste-U-dual.txt"),
("minirec2.txt","minirec1.txt"),
("gchat1.txt","gchatrev1.txt"),
("stateidx1.txt","stateidx2.txt"),
("july2.txt","july1.txt"),
("decidablilityex2.txt","decidablilityex1.txt"),
("ctxtb2.txt","ctxtb1.txt"),
("ctxta2.txt","ctxta1.txt"),
("september2.txt","september1.txt"),
("innerloop2-dual.txt","innerloop1-dual.txt"),
("expensive2.txt","expensive1.txt"),
("aaaaaab2.txt","aaaaaab1.txt"),
("badseq2.txt","badseq1.txt"),
("ex1.txt","ex2.txt"))
