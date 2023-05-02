enum SessionType {
  case Rec(rvar: String, t:SessionType)
  case Var(name: String)
  case Choice(list: List[(Char,String,SessionType)])
  case End

  def isWellFormed() = isWellFormedAux(this,List())

  private def isWellFormedAux(st:SessionType, env:List[String]):Boolean = st match
    case End => true
    case Rec(v,st1) => isContractive(st1,List(v)) && isWellFormedAux(st1,v::env)
    case Var(v) => env.contains(v)
    case Choice(list) => if list.isEmpty then false
      else (list.head._1.equals('!') || list.head._1.equals('?')) && list.forall(x => x._1.equals(list.head._1)) &&
        noDuplicates(list.map(x => (x._2))) && list.forall(x => isWellFormedAux(x._3,env))

  private def noDuplicates(list:List[String]):Boolean = 
  list match
      case Nil => true
      case x::list1 => !list1.contains(x) && noDuplicates(list1)
      
  private def isContractive(st:SessionType,list:List[String]):Boolean = st match
  	case Rec(v,st1) => isContractive(st1,v::list)
  	case Var(v) => !list.contains(v)
  	case _ => true
      
  def toAutomaton() = {
  	val x = toAutomatonAux(this,0,List(),Map())
  	new Automaton(x._1,x._2)
  }
  
  private def toAutomatonAux(st:SessionType,s:Int,edges:List[(Int,Char,String,Int)],env:Map[String,Int]):(Int,List[(Int,Char,String,Int)]) = st match
  	case End => (s + 1,edges)
  	case Var(v) => (s,edges)
  	case Rec(v,st1) => toAutomatonAux(st1,s,edges,env+(v -> head(st1,s,env)))
  	case Choice(list) => {
  		var s1 = s + 1
  		var edges1 = edges
  		var s2 = 0;
  		var x = (0,List[(Int, Char, String, Int)]())
  		for((c,str,st1)<-list)
  		{
  			s2 = head(st1,s1,env)
  			x = toAutomatonAux(st1,s1,edges1,env)
  			s1 = x._1
  			edges1 = (s,c,str,s2)::x._2  
  		} 
  		(s1,edges1)
  	}
  	
  private def head(st:SessionType,s:Int,env:Map[String,Int]) = st match
  	case Var(v) => env(v)
  	case _ => s
}
