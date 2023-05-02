//import InTree.Leaf

enum InTree {
    case Leaf(x: Int)
    case Node(lab: String, T: InTree)
    
    def toList():List[String] = this match
      case Leaf(_) => Nil
      case Node(lab,t1) => lab::t1.toList()
}


object InTree {
  def leaf(T: InTree): Int = T match
    case Leaf(x) => x
    case Node(lab, t) => leaf(t)

  def leafs(set: Set[InTree]) = set.map(leaf)
}
