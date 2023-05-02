import scala.collection.immutable.Set
import java.io.File
import java.io.PrintWriter
import scala.sys.process._

class Automaton(s:Int, e:List[(Int,Char,String,Int)]) {

  val size = s

  val edges = e

  def outgoing(state:Int) = e.filter((s,_,_,_) => s.equals(state))

  def isEnd(state:Int) = outgoing(state).isEmpty
  
  def isEndTree(state:Set[InTree]) = if (state.size == 1) then state.head match
      case InTree.Node(_,_) => false
      case InTree.Leaf(s) => isEnd(s)
    else false

  def isSending(state:Int) = !isEnd(state) &&
    outgoing(state).foldRight(true)((x: (Int, Char, String, Int), b: Boolean) => x._2.equals('!') && b)

  def isReceiving(state: Int) = !isEnd(state) &&
    outgoing(state).foldRight(true)((x: (Int, Char, String, Int), b: Boolean) => x._2.equals('?') && b)
    
  def in(state:Int) = outgoing(state).flatMap((_,c,s,_) => if c.equals('?') then Some(s) else None).toSet
  
  def out(state:Int) = outgoing(state).flatMap((_,c,s,_) => if c.equals('!') then Some(s) else None).toSet
  
  def inAndRed(state:Int) = outgoing(state).flatMap((_,c,lab,s) => if c.equals('?') then Some((lab,s)) else None).toSet
  
  def outAndRed(state:Int) = outgoing(state).flatMap((_,c,lab,s) => if c.equals('!') then Some((lab,s)) else None).toSet
  
  def inT(s: Set[InTree]):Set[String] = s.flatMap(t => t match
                                           case InTree.Leaf(n) => in(n)
                                           case InTree.Node(lab,_) => Set(lab))
  
  def outT(s: Set[InTree]):Set[String] = {
    val aux = s.flatMap(t => inTree(InTree.leaf(t))).flatMap(s => InTree.leafs(s))
    if aux.isEmpty then Set()
    else{
      aux.tail.foldRight(out(aux.head))((n,set) => out(n) & set)
    }
  }
  
  def toDotAux() = {
    val dq = Integer.parseInt("22",16).toChar //dq=";
    var out = "digraph {\n node [shape=point] ENTRY\n node [shape=circle]\n ENTRY -> 0\n";
    for ((s1,c,lab,s2) <- e) out = out + s1 + " -> "  + s2 + " [label=" + dq + c + lab + dq + "]\n";  
    out = out + "}";
    out
  }
  
  def toDot(pathDot:String,pathSvg:String) = {
    val file_Object = new File(pathDot) 
    val print_Writer = new PrintWriter(file_Object) 
    val out = toDotAux() 
    print_Writer.write(out) 
    print_Writer.close()
    val command = "dot -Tsvg " + pathDot + " -o " + pathSvg;
    //println(command);
    val pb = Process(command);
    pb.!;
   }

  def cycle(c:Char,state: Int) = cycleAux(c,state,Set.empty)

  private def cycleAux(c:Char, state: Int, Acc:Set[Int]):Boolean = {
    if Acc.contains(state) then true
    else outgoing(state).filter((_, c1, _, _) => c1.equals(c)).exists(x => cycleAux(c, x._4, Acc + state))
  }
  
  def dual() = new Automaton(s,e.map((st1,c,lab,st2) => (st1,if c.equals('!') then '?' else '!',lab,st2)))
  
  def append(lab:String,set:Set[InTree]):Set[InTree] = set.map(T => InTree.Node(lab,T))

  def inTree(state: Int):Option[Set[InTree]] = {
    if cycle('?',state) then None
    else Some (inTreeAux(state))
  }

  def inTreeAux(state: Int):Set[InTree] = {
      val l = outgoing(state).filter((_, c, _, _) => c.equals('?'))
      if l.isEmpty then Set(InTree.Leaf(state))
      else (l.flatMap((_,_,lab,s2) => append(lab,inTreeAux(s2)))).toSet
  }

  def applySubst(subst:Map[Int,Set[InTree]],T:InTree):Set[InTree] = T match
    case InTree.Leaf(s) => if subst.contains(s) then subst(s) else Set(InTree.Leaf(s))
    case InTree.Node(lab, t1) => append(lab,applySubst(subst,t1))

  def buildSubst(c: Char, lab: String) = {
    var r = Map[Int,Set[InTree]]()
    for (p <- edges.filter((_, c1, lab1, _) => c1.equals(c) && lab1.equals(lab)).map((s1, _, _, s2) => (s1, s2))) r = r+(p._1 -> Set(InTree.Leaf(p._2)))
    r
  }

  def step(T:InTree,c:Char,lab:String):Option[Set[InTree]] = {
    if c.equals('?') then T match
      case InTree.Leaf(s) => {
        val list = outgoing(s).filter((_,c1,lab1,_) => c1.equals(c) && lab1.equals(lab))
        if list.isEmpty then None
        else Some(Set(InTree.Leaf(list(0)._4)))
      }
      case InTree.Node(lab1,t) => if lab1.equals(lab) then Some(Set(t)) else None
    else {//TODO: handle the case c != '!'
      val m = buildSubst(c, lab)
      val s = InTree.leaf(T)
      val r = inTree(s)
      r match
        case None => None
        case Some(set) =>
          if InTree.leafs(set).forall(m.contains) then
          //val m1 = Map(s -> (set.map(T1 => applySubst(m, T1)).foldRight(Set())((s1:Set[InTree], s2:Set[InTree]) => s1.union(s2))))
          Some(applySubst(Map(s -> (
            set.map(T1 => applySubst(m, T1)).foldRight(Set())((s1:Set[InTree], s2:Set[InTree]) => s1.union(s2)))),T))
          else None
    }
  }

  def inTreeOfList(list: List[String],s: Int):InTree = list match
    case List() => InTree.Leaf(s)
    case lab::list1 => InTree.Node(lab, inTreeOfList(list1,s))

  def treeOfState(state: Int) = InTree.Leaf(state)

  def optToSet[T](PSet: Option[Set[T]]):Set[T] = PSet match
    case None => Set()
    case Some(set) => set

  def stepS(P:Option[Set[InTree]],c:Char,lab:String):Option[Set[InTree]] =
    P.map(set =>set.flatMap(T => step(T,c,lab)).flatten).flatMap(set => if set.isEmpty then None else Some(set))
    
  def wideningPoints() = e.filter((s1,_,_,s2) => s2 <= s1).map(x => x._4).toSet
}
