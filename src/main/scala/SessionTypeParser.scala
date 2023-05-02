import scala.util.parsing.combinator.*

class SessionTypeParser extends RegexParsers {

  private val reserved = Set("end","End","Rec","rec")

  def varExpr: Parser[String] = """[a-zA-Z]+[a-zA-Z0-9]*""".r.filter(s => !reserved.contains(s))

  def recK: Parser[String] = "Rec" | "rec"

  def mode: Parser[Char] =  '!' | '?' ^^ {_.toChar}

  def cmode: Parser[Char] =  '+' | '&' ^^ {_.toChar}

  def label: Parser[String] = """[a-zA-Z]+[a-zA-Z0-9]*""".r.filter(s => !reserved.contains(s))

  def rvar: Parser[SessionType] = varExpr ^^ {s => SessionType.Var(s)}

  def rec: Parser[SessionType] = (recK ~ varExpr ~ ".\\s*".r ~ sessionType) ^^ {case r ~ x ~ dot ~ t => SessionType.Rec(x,t)}

  def end: Parser[SessionType] = "end" ^^ {_ => SessionType.End} | "End" ^^ {_ => SessionType.End}

  //def act: Parser[SessionType.Act] = (mode ~ label ~ ";" ~ sessionType) ^^ {case m ~ l ~ sep ~ t => SessionType.Act(m,l,t)}
  def act: Parser[(Char,String,SessionType)] = (mode ~ label ~ ";\\s*".r ~ sessionType) ^^ {case m ~ l ~ sep ~ t => (m,l,t)} | (mode ~ label) ^^ {case m ~ l => (m,l,SessionType.End)}
  //def act2: Parser[(Char,String,SessionType)] = (mode ~ label) ^^ {case m ~ l => (m,l,SessionType.End)}

  def actL: Parser[List[(Char,String,SessionType)]] = repsep(act,",\\s*".r) //^^ {case list => act::list.map(x => x._2)}
  //def actL: Parser[List[SessionType.Act]] = act ~ rep("," ~ act) ^^ {case act ~ list => act::list.map(x => x._2)}
    //^^ {x => List(x)} | act ~ "," ~ actL ^^ {case a ~ sep ~ a1 => a::a1}

  def choice: Parser[SessionType] = cmode ~ """[""" ~ actL ~ """]""" ^^ {case m ~ sep1 ~ list ~ sep2 => SessionType.Choice(list)} |
    """[""" ~ actL ~ """]""" ^^ {case sep1 ~ list ~ sep2 => SessionType.Choice(list)} | act ^^ {r => SessionType.Choice(List(r))} |
    cmode ~ """{""" ~ actL ~ """}""" ^^ {case m ~ sep1 ~ list ~ sep2 => SessionType.Choice(list)} |
    """{""" ~ actL ~ """}""" ^^ {case sep1 ~ list ~ sep2 => SessionType.Choice(list)}

  def sessionType[SessionType] = end | rec | rvar | choice
}
