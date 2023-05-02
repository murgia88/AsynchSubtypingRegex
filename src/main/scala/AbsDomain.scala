import scala.io.StdIn.readLine
import scala.annotation.tailrec

enum AbsDomain {

  case Ch(set:Set[String])
  case R(l:List[AbsDomain])
  case Star(d:AbsDomain)
  
  def isEmpty() = this match
    case R(list) => list.isEmpty
    case _ => false
  
  def head():AbsDomain = this match
    case R(p::list) => p.head()
    case _ => this
    
  def tail() = this match
    case R(e::list) => R(list)
    case _ => R(Nil)
    
  def last():AbsDomain = this match
    case R(p::list) => if (list.isEmpty) then p.last() else list.last
    case _ => this
    
  def remLast():AbsDomain = this match
    case R(list) => if (list.isEmpty) then R(Nil) else R(list.slice(0,list.length - 2))
    case _ => R(Nil)
    
  def length() = this match
    case R(list) => list.length
    case _ => 1
    
  def chars():Set[String] = this match
    case R(p::list) => p.chars().union((R(list)).chars())
    case Star(p) => p.chars()
    case Ch(set) => set
    case _ => Set()
    
  def norm():AbsDomain = this match
    case Ch(set) => this
    case Star(Star(p)) => Star(p).norm()
    case Star(R(Nil)) => R(Nil)
    case Star(p) => Star(p.norm())
    case R(p::Nil) => p.norm()
    case R(list) => R(list.map(x => x.norm())).flat()
    
  def flat():AbsDomain = this match
    case R(Nil) => this
    case R(p::list) => AbsDomain.concat(p,R(list).flat())
    case _ => this
    
  def star:Option[AbsDomain] = this match
    case Ch(_) => None
    case Star(p) => Some(p)
    case R(Nil) => None
    case R(p::list) => if R(list).isEps() then p.star else None
    //case _ => None
    
  def isStar():Boolean = this match
    case Star(_) => true
    case R(p::list) => if R(list).isEps() then p.isStar() else false
    case _ => false
    
  def starHeight():Int = this match
    case Ch(_) => 0
    case Star(p) => p.starHeight() + 1
    case R(list) => list.map(_.starHeight()).fold(0)((x,y) => if (x>y) then x else y)

  def subseteq(q:AbsDomain):Boolean = AbsDomain.subseteq(Set(this),Set(q))
    
  def isAccepting():Boolean = this match
    case Star(_) => true
    case Ch(_) => false
    case R(Nil) => true
    case R(p::list) => if p.isAccepting() then R(list).isAccepting() else false
    
  def isEps():Boolean = this match
    case Ch(_) => false
    case Star(p) => p.isEps()
    case R(list) => list.foldRight(true)((p,acc) => acc && p.isEps())
    
  def trans(lab:String): Set[AbsDomain] = this match
    case Ch(chars) => if chars.contains(lab) then Set(R(Nil)) else Set()
    case Star(p) => p.trans(lab).map(AbsDomain.concat(_,this))
    case R(Nil) => Set()
    case R(p::list) => if p.isEps() then R(list).trans(lab) 
                       else if p.isAccepting() then p.trans(lab).map(AbsDomain.concat(_,R(list))) | R(list).trans(lab)
                       else p.trans(lab).map(AbsDomain.concat(_,R(list)))
                       
  def immActions(): Set[String] = this match
    case Ch(chars) => chars
    case Star(p) => p.immActions()
    case R(Nil) => Set()
    case R(p::list) => if p.isAccepting() then {
                           val pImm = p.immActions() 
                           val lImm = R(list).immActions()
                           pImm | lImm
                           }
                       else p.immActions() 
                       
  def toEpsAutomaton(first:Int):(Int,Set[(Int,String,Int)],Set[Int]) = this match
    case Ch(chars) => (first + 2,chars.map(lab => (first,lab,first + 1)),Set(first + 1))
    case Star(p) => {
      val aut = p.toEpsAutomaton(first)
      (aut._1,aut._2 | aut._3.map(fstate => (fstate,"",first)),Set(first))
      //(aut._1,aut._2 | Set((aut._1 - 1,"",first)),Set(first))
    }
    case R(Nil) => (first + 1,Set(),Set(first))
    case R(p::list) => {
      val aut = p.toEpsAutomaton(first)
      val aut2 = R(list).toEpsAutomaton(aut._1)
      (aut2._1,aut._2 | aut2._2 | aut._3.map(fstate => (fstate,"",aut._1)),aut2._3)
    }

  def toAutomaton() = {
    val epsAut = toEpsAutomaton(0)
    AbsDomain.remEps(epsAut._1,epsAut._2,epsAut._3)
  }
  
  def print:String = if isEps() then "Eps" else this match
    case Ch(chars) => "{" + chars.foldRight("")((string,acc) => if acc.equals("") then string else string + "," + acc) + "}"
    case Star(p) => "(" + p.print + ")*"
    case R(list) => list.map(_.print).foldRight("")((string,acc) => if acc.equals("") then string else string + "." + acc)
}

object AbsDomain{
  def abs(t:InTree):AbsDomain = R(t.toList().map(s => Ch(Set(s))))
      
  def concat(p1:AbsDomain,p2:AbsDomain) = (p1,p2) match
     case (R(list1),R(list2)) => R(list1:::list2)
     case (R(list),_) => R(list:::List(p2))
     case (_,R(list)) => R(p1::list)
     case (_,_) => R(List(p1,p2))

  def absConcat(p1:AbsDomain,p2:AbsDomain):AbsDomain = {
    val l = p1.last()
    val f = p2.head()
    if (l.isStar() && f.isStar()) then concat(p1.remLast(),concat(concatUtil(l,f),p2.tail())) else concat(p1,p2)
  }
     
  def concatUtil(p1:AbsDomain,p2:AbsDomain):AbsDomain = if (p1.equals(p2)) then p1 else Star(Ch(p1.chars() | p2.chars()))
   
  def wide(p1:AbsDomain,q1:AbsDomain,p2:AbsDomain,q2:AbsDomain,k:Int):AbsDomain ={
    if q1.isEmpty() || q2.isEmpty() then mash(concat(p1,q1),concat(p2,q2),k) else{
      val a1 = q1.head()
      val r1 = q1.tail()
      val a2 = q2.head()
      val r2 = q2.tail()
      if a1.equals(a2) && a1.starHeight() <= k then {
      val m = mash(p1,p2,k)
      val w = wide(R(List()),r1,R(List()),r2,k)
      absConcat(m,absConcat(a1,w))
      }
      else if a1.equals(a2) && a1.starHeight() > k then {
        wide(concat(p1,a1),r1,concat(p2,a2),r2,k)
      }
      else {if q1.length() > q2.length() then wide(concat(p1,a1),r1,p2,q2,k)
      	    else wide(p1,q1,concat(p2,a2),r2,k)}
    }}
    
  def wideSet(t1:Set[AbsDomain],t2:Set[AbsDomain],k:Int) = if t1.isEmpty then t2 else if t2.isEmpty then t1 else t1.flatMap(p => t2.map(q => wide(R(Nil),p,R(Nil),q,k)))
    
  def close(set:Set[AbsDomain]):Set[AbsDomain] = if set.isEmpty then set 
    else{
      val seti = set.map(_.norm())
      val p = seti.head
      val set1 = seti.tail
      val set2 = set1.filter(q => !q.subseteq(p))
      if set2.exists(q => p.subseteq(q)) then close(set2) else Set(p) | close(set2)
    } 
    
  def mash(p:AbsDomain,q:AbsDomain,k:Int):AbsDomain = {
    if p.isEmpty() && q.isEmpty() then R(List())
    else if p.isEmpty() && Star(q).starHeight() <= k then Star(q)
    else if q.isEmpty() && Star(q).starHeight() <= k then Star(p)
    else if k >= 2 then { (p.star,q.star) match
      case(Some(p1),Some(q1)) => Star(wide(R(List()),p1,R(List()),q1,k))
      case(Some(p1),_) => Star(wide(R(List()),p1,R(List()),q,k))
      case(_,Some(q1)) => Star(wide(R(List()),p,R(List()),q1,k))
      case(_,_) =>{ Star(Ch(p.chars().union(q.chars())))}}
    else Star(Ch(p.chars().union(q.chars())))
   }
   
   def transSet(set:Set[AbsDomain],word:List[String]):Set[AbsDomain] = word match
     case Nil => set
     case lab::word1 => transSet(set.flatMap(p => p.trans(lab)),word1)
     
   def remEps(size:Int,trans:Set[(Int,String,Int)],accept:Set[Int]):(Int,Set[(Int,String,Int)],Set[Int]) = {
     val epsM = (0 to size - 1).toList.map(s => epsClosure(Set(s),trans))
     val newSize = epsM.toSet.size
     val newAccept = (0 to size - 1).toSet.filter(n => !(epsClosure(Set(n),trans) & accept).isEmpty)
     val filteredTrans = trans.filter((_,l,_) => !l.equals(""))
     val newTrans = (0 to size - 1).toSet.flatMap(n => filteredTrans.filter((s,l,si) => epsClosure(Set(n),trans).contains(s)).map((s,l,si) => (n,l,epsM.indexWhere(x => x.equals(epsClosure(Set(si),trans))))))
     (newSize,newTrans,newAccept)
   }
       
   def epsClosure(old:Set[Int],trans:Set[(Int,String,Int)]):Set[Int] =
     val mynew = old | trans.flatMap(t => if old.contains(t._1) && t._2.equals("") then Set(t._3) else Set())
     if mynew.subsetOf(old) then old else epsClosure(mynew,trans)
     
   def automatonOfSet(set:Set[AbsDomain]):(Int,Set[(Int,String,Int)],Set[Int]) = {
     val f = (p:AbsDomain,aut:(Int,Set[(Int,String,Int)],Set[Int])) => {
       val aut1 = p.toEpsAutomaton(aut._1)
       (aut1._1,aut._2 | aut1._2 | Set((0,"",aut._1)),aut._3 | aut1._3)
     }
     val aut = set.foldRight((1,Set(),Set()))(f)
     val ret = remEps(aut._1,aut._2,aut._3)
     ret
   }
     
   def subseteq(set1:Set[AbsDomain],set2:Set[AbsDomain]):Boolean ={
     subseteqAux(scala.collection.mutable.Set(),automatonOfSet(set1),automatonOfSet(set2),Set(0),Set(0))}
     
   def subseteqAux(acc:scala.collection.mutable.Set[(Set[Int],Set[Int])],aut1:(Int,Set[(Int,String,Int)],Set[Int]),aut2:(Int,Set[(Int,String,Int)],Set[Int]),s1:Set[Int],s2:Set[Int]):Boolean = { 
     if s1.isEmpty then true 
     else if s2.isEmpty then false
     else if acc.contains((s1,s2)) then true
     else if !(aut1._3 & s1).isEmpty && (aut2._3 & s2).isEmpty then false
     else {
       acc.add((s1,s2))
       val trans1 = aut1._2.filter(x => s1.contains(x._1))
       val labs1 = trans1.map(_._2)
       if labs1.isEmpty then {true} else{
       val trans2 = aut2._2.filter(x => s2.contains(x._1))
       val ret = labs1.forall(lab => subseteqAux(acc,aut1,aut2,
         trans1.filter(x => x._2.equals(lab)).map(_._3),
         trans2.filter(x => x._2.equals(lab)).map(_._3)))
       ret
       }
     }
   }
} 
