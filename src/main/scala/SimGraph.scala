import java.io.File
import java.io.PrintWriter
import scala.sys.process._

class SimGraph(Automaton1:Automaton,Automaton2:Automaton){ //,States:Set[(Int,Set[(AbsDomain,Int)])],Transitions:Set[(Int,Set[(AbsDomain,Int)]),Char,String,(Int,Set[(AbsDomain,Int)])]) {

  val A1 = Automaton1
  val A2 = Automaton2
  var last = Set[(Int,Set[(AbsDomain,Int)])]()

  def outStep(s:Int,t:Set[(AbsDomain,Int)],lab:String):(Int,Set[(AbsDomain,Int)]) = if !A1.out(s).contains(lab) then (-1,Set()) else {
    val newS = A1.outgoing(s).filter(x => x._2.equals('!') && x._3.equals(lab)).head._4
    val newT = t.flatMap((p,s1) => {
      val aux = A2.stepS(Some(Set(A2.treeOfState(s1))),'!',lab)
      aux match
        case None => Set()
        case Some(set) => {
          val ret = (set.map(tr => (AbsDomain.concat(p,AbsDomain.abs(tr)),InTree.leaf(tr)))) 
          ret
        }
    })
    (newS,newT)
  }
  
  def inStep(s:Int,t:Set[(AbsDomain,Int)],lab:String):(Int,Set[(AbsDomain,Int)]) = if !A1.in(s).contains(lab) then (-1,Set()) else {
    val newS = A1.outgoing(s).filter(x => x._2.equals('?') && x._3.equals(lab)).head._4
    val newT = t.flatMap((p,s1) => 
      if (p.isAccepting() && A2.in(s1).contains(lab)) then (p.trans(lab).map(q => (q,s1)) | A2.outgoing(s1).filter(
        x => x._2.equals('?') && x._3.equals(lab)).map(x => (AbsDomain.R(Nil),x._4)).toSet)
                                                      else p.trans(lab).map(q => (q,s1)))
    (newS,newT)
  }
  
  def steps(s:Int,t:Set[(AbsDomain,Int)],labs:List[(Char,String)]):(Int,Set[(AbsDomain,Int)]) = labs match
    case Nil => (s,t)
    case (c,lab)::labs1 => if c.equals('!') then { val red = outStep(s,t,lab); steps(red._1,red._2,labs1)}
                                            else { val red = inStep(s,t,lab); steps(red._1,red._2,labs1)}
                                            
  def outAbs(t:Set[(AbsDomain,Int)]):Set[String] = if t.isEmpty then Set() else{
    val outs = t.map(x => A2.inTree(x._2).map(y => A2.outT(y)))
    outs.head match
      case None => Set()
      case Some(set) => outs.tail.foldLeft(set)((seti,opt) => opt match
        case None => Set()
        case Some(setii) => seti & setii)
  }
    
  def inAbs(t:Set[(AbsDomain,Int)]):Set[String] = {
    val ins = t.map(x => if x._1.isAccepting() then{val pIn = x._1.immActions(); val sIn = A2.in(x._2); if sIn.isEmpty then Set() else pIn | sIn}
                         else {x._1.immActions()})
    if ins.contains(Set()) then Set()
    else ins.foldRight(Set())(_|_)
  }
  
  def isErr(s:Int,t:Set[(AbsDomain,Int)]) =
    if A1.cycle('!',s) && t.exists((p,_) => !p.isEps()) then true
    else if A1.isSending(s) then !A1.out(s).subsetOf(outAbs(t))
    else if A1.isReceiving(s) then {val inSet = inAbs(t); inSet.isEmpty || !inSet.subsetOf(A1.in(s))}
    else !(A1.isEnd(s) && t.forall((p,ss) => p.isEps() && A2.isEnd(ss))) 
    
  def merge(t:Set[(Int,Set[(AbsDomain,Int)])]):Set[(Int,Set[(AbsDomain,Int)])] = {
    val ret = t.groupBy(_._1).map((k,x) => (k,x.flatMap(_._2))).toSet
    ret
  }
    
  def step(s:Int,t:Set[(AbsDomain,Int)]):Set[(Int,Set[(AbsDomain,Int)])] = {
    if isErr(s,t) then Set()
    else if A1.isSending(s) then {merge(A1.out(s).map(lab => outStep(s,t,lab)))}
    else if A1.isReceiving(s) then {merge(inAbs(t).map(lab => inStep(s,t,lab)))}
    else Set()
  }
  def get(t:Set[(AbsDomain,Int)],s:Int):Set[AbsDomain] = {
    t.filter(x => x._2 == s).map(_._1)
  }
    
  def wide(t1:Set[(AbsDomain,Int)],t2:Set[(AbsDomain,Int)],k:Int) = {
    val st = t1.map(_._2) | t2.map(_._2)
    st.flatMap(s => AbsDomain.wideSet(get(t1,s),get(t2,s),k).map(p => (p,s)))
  }
  
  def close(set:Set[(AbsDomain,Int)]):Set[(AbsDomain,Int)] = {
    val st = set.map(_._2)
    st.flatMap(s => AbsDomain.close(get(set,s)).map(p => (p,s)))
  }
  
  def closeMap(m:Map[Int,Set[(AbsDomain,Int)]]):Map[Int,Set[(AbsDomain,Int)]] = m.mapValues(x => close(x)).toMap
  
  def wideMap(m1:Map[Int,Set[(AbsDomain,Int)]],m2:Map[Int,Set[(AbsDomain,Int)]],wp:Set[Int],k:Int):Map[Int,Set[(AbsDomain,Int)]] =
    closeMap(wp.map(s => (s,wide(m1.getOrElse(s, Set()),m2.getOrElse(s, Set()),k))).toMap.filter((key,t) => !t.isEmpty))
    
  def subsetAbsTree(t1:Set[(AbsDomain,Int)],t2:Set[(AbsDomain,Int)]):Boolean =
    (0 to A2.size - 1).toList.forall(s => AbsDomain.subseteq(get(t1,s),get(t2,s)))    
  
  def buildSimGraphAux(k:Int,depth:Int,f:Map[Int,Set[(AbsDomain,Int)]],states:Set[(Int,Set[(AbsDomain,Int)])],wp:Set[Int],
    ret:List[Set[(Int,Set[(AbsDomain,Int)])]]):(List[Set[(Int,Set[(AbsDomain,Int)])]],returnType) = if depth == 0 then (ret,returnType.Maybe) else 
      if states.exists((s,t) => isErr(s,t)) then {(ret,returnType.False)} else {
      val statesNewAux:Set[(Int,Set[(AbsDomain,Int)])] = merge(states | states.flatMap((s,t)=>step(s,t))).map((s,t) => (s,close(t))) 
      val f1 = statesNewAux.groupBy(_._1).map((key,x) => (key,x.flatMap(_._2))).view.filterKeys(key => wp.contains(key)).toMap
      val newF = wideMap(f,f1,wp,k)
      val statesNew = merge(statesNewAux.map((s,t) => (s,newF.getOrElse(s,t))) | newF.toSet)
      if statesNew.exists((s,t) => isErr(s,t)) then{(ret:::List(statesNew),returnType.False)}
      else if statesNew.forall((s,t) => states.exists((s1,t1) => s == s1 && subsetAbsTree(t,t1))) then (ret:::List(statesNew),returnType.True)
      else buildSimGraphAux(k,depth-1,newF,statesNew,wp,ret:::List(statesNew))
    }
    
  def buildSimGraph(k:Int,depth:Int):(List[Set[(Int,Set[(AbsDomain,Int)])]],returnType) = {
    val c0:Set[(Int,Set[(AbsDomain,Int)])] = Set((0,Set((AbsDomain.R(Nil),0))))
    val wp = A1.wideningPoints()
    val m = if wp.contains(0) then Map[Int,Set[(AbsDomain,Int)]](0 -> Set((AbsDomain.R(Nil),0))) else Map[Int,Set[(AbsDomain,Int)]]()
    val ret = buildSimGraphAux(k,depth,m,c0,wp,List(c0))
    last = ret._1.last
    ret
  }
  
  def getFromLast(s:Int):(Int,Set[(AbsDomain,Int)]) = last.filter((s1,t) => s1 == s).head
  
  def toGraph():
  (Set[(Int,Set[(AbsDomain,Int)])],Set[(Int,String,Int)]) =
  {
    val states = last.map((i,s) => (i,close(s)))
    //val states = last
    val trans = last.filter((s,t) => !isErr(s,t)).flatMap((s,t) => if A1.isSending(s) then A1.outgoing(s).map((_,_,lab,s1) => (s,"!"+lab,s1))
                                                                   else A1.outgoing(s).filter(x => inAbs(t).contains(x._3)).map((_,_,lab,s1) => (s,"?"+lab,s1)))
    (states,trans)
  }
  
  def printConf(x:(Int,Set[(AbsDomain,Int)])):String = x._1 + " < " + x._2.map((p,s1) => p.print + "." + s1 + "\n").foldRight("")(_+_) 
  
  def toDot(pathDot:String,pathSvg:String) = {
    val (states,transitions) = toGraph()
    val file_Object = new File(pathDot) 
    val print_Writer = new PrintWriter(file_Object) 
    val string = "digraph {\n node [shape=point] ENTRY\n node [shape=circle]\n" + states.foldRight("")((conf,acc) => acc + conf._1 + " [label=" + STree.dq + printConf(conf) + STree.dq + (if isErr(conf._1,conf._2) then 
                                                                                                                                               " color=" + STree.dq + "red" + STree.dq  
                                                                                                                                          else "") 
                                 + "]\n") + 
                                 (if states.isEmpty then "" else "ENTRY -> 0\n") +
                                 transitions.foldRight("")((x,acc) => acc + x._1 + " -> " + x._3 + " [label=" + STree.dq + x._2 + STree.dq + "]\n") + "\n}"
    print_Writer.write(string) 
    print_Writer.close()
    val command = "dot -Tsvg " + pathDot + " -o " + pathSvg;
    //println(command);
    val pb = Process(command);
    pb.!;
  }
}

enum returnType {
  case True
  case False
  case Maybe
}

object returnType{
  def fromBool(b:Boolean) = if b then True else False
}
