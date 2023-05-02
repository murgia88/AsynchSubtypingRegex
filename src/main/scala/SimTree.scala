class SimTree(Automaton1:Automaton,Automaton2:Automaton) {

  val A1 = Automaton1
  val A2 = Automaton2

  def buildSimTree(s1: Int, s2: Option[Set[InTree]], depth: Int): STree = {
  if depth == 0 then STree.Leaf(s1, s2)
  else s2 match
    case None => STree.Leaf(s1, s2)
    case Some(set) => if A1.isEnd(s1) then STree.Leaf(s1, s2)
      else if A1.isSending(s1) then if !A1.out(s1).subsetOf(A2.outT(set)) then STree.Leaf(s1, s2)
                                    else {
                                    STree.Node(s1,s2,A1.outgoing(s1).map((_, c, lab, s11) => (c, lab, buildSimTree(s11, A2.stepS(s2, c, lab), depth - 1))))}
      else if A1.isReceiving(s1) then {
      			  val in2 = A2.inT(set)
      			  if !in2.subsetOf(A1.in(s1))then STree.Leaf(s1, s2)
      		          else STree.Node(s1,s2,A1.outgoing(s1).filter((_, c, lab, s11) => in2.contains(lab)).map((_, c, lab, s11) => (c, lab, buildSimTree(s11, A2.stepS(s2, c, lab), depth - 1))))
      }
      else {
        println("Mixed state found: " + s1)
        STree.Leaf(s1, s2)
      }
  }
  
  def hasErrors(s:STree):Boolean = s match
    case STree.Node(_,_,list) => list.exists(x => hasErrors(x._3))
    case STree.Leaf(s1,s2) => s2 match
        case None => true
        case Some(set) => if A1.isEnd(s1) then !A2.isEndTree(set)
          else if A1.isSending(s1) then !A1.out(s1).subsetOf(A2.outT(set))
          else if A1.isReceiving(s1) then 
            val in2 = A2.inT(set);
            !in2.subsetOf(A1.in(s1)) 
          else true     		        	
}
