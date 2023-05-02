import java.io.File
import java.io.PrintWriter
import scala.sys.process._

enum STree {
  case Leaf(s1: Int, s2: Option[Set[InTree]])
  case Node(s1: Int, s2: Option[Set[InTree]],list: List[(Char, String, STree)])
  
  private def toDotAux(str:String,n:Int):(String,Int) = this match
    case STree.Leaf(_,_) => (str + n + "[label=" + STree.dq + STree.stringOfConf(this) + STree.dq + "]\n",n + 1)
    case STree.Node(q,s,list) => {
      var ret = (str + n + "[label=" + STree.dq + STree.stringOfConf(this) + STree.dq + "]\n",n+1);
      //var nn = n+1;
      for ((c,lab,st) <- list){  
        ret = (ret._1 + n + " -> " + ret._2 + " [label=" + STree.dq + c + lab + STree.dq + "]\n",ret._2); 
        ret = st.toDotAux(ret._1,ret._2);
      }
      ret 
    }
    
  def toDot(pathDot:String,pathSvg:String) = {
    val file_Object = new File(pathDot) 
    val print_Writer = new PrintWriter(file_Object) 
    val (out,n) = toDotAux("digraph {\n",0)
    print_Writer.write(out+"\n}") 
    print_Writer.close()
    val command = "dot -Tsvg " + pathDot + " -o " + pathSvg;
    val pb = Process(command);
    pb.!;
   }
  def genSequence(state:Int,maxDepth:Int):List[Option[Set[InTree]]] = if maxDepth < 0 then List() else this match
    case Leaf(s1,s2) => if s1 == state then List(s2) else List()
    case Node(s1, s2,list) => (if s1 == state then List(s2) else List()):::list.map(x => x._3.genSequence(state,maxDepth- 1)).foldRight(List())(STree.merge)
}

object STree{
  val dq = Integer.parseInt("22",16).toChar //dq=";
  
  def stringOfTree(t:InTree):String = t match
    case InTree.Leaf(q) => q.toString()
    case InTree.Node(lab,t1) => lab + "." + stringOfTree(t1)
  
  def stringOfOST(s:Option[Set[InTree]]) = s match
    case None => "None"
    case Some(set) => set.map(t => stringOfTree(t) + "\n").foldRight("")((s1,s2) => s1 + s2)
    
  def stringOfConf(st:STree) = st match
    case STree.Leaf(q,s) => q + " < " + stringOfOST(s)
    case STree.Node(q,s,_) => q + " < " + stringOfOST(s)
    
  private def merge(l1: List[Option[Set[InTree]]],l2:List[Option[Set[InTree]]]):List[Option[Set[InTree]]] = (l1,l2) match
    case (Nil,_) => l2
    case (_,Nil) => l1
    case (o1::l1i,o2::l2i) => o1.flatMap(x => o2.map(y => x.union(y)))::merge(l1i,l2i)
}
