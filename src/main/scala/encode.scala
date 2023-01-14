package encode

import scala.collection.mutable.{Buffer, Map => MutableMap, Queue}
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

object TeXPEGs extends RegexParsers with PackratParsers {
	lazy val id = """[@A-Za-z]+\*?""".r
	lazy val yen = "\\" ~> id ^^ YenTeX
	lazy val cmt = "%" ~ ".*".r ^^ (_ => StrTeX(""))
	lazy val str = """[^\\\{\}\[\]%\$]+""".r ^^ StrTeX
	lazy val bra = "{" ~> doc <~ "}" ^^ ArgTeX
	lazy val sqb = "[" ~> doc <~ "]" ^^ OptTeX
	lazy val esc = "\\" ~> """[\\{}_&%\$#\|;!, ]""".r ^^ EscTeX
	lazy val env = ("\\begin{" ~> id <~ "}") ~ arg ~ doc <~ ("\\end{" ~> id <~ "}") ^^ {
		case name ~ args ~ body => EnvAppTeX(name, args, body)
	}
	lazy val cmd = defApp | (not("\\end{") ~> yen ~ arg ^^ {
		case name ~ args => CmdAppTeX(name, args)
	})
	lazy val mat = "$" ~> (esc | cmd | bra | sqb | str).* <~ "$" ^^ DocTeX ^^ MatTeX
	lazy val defCmd = "\\" ~> (
		"NewDocumentCommand" |
		"RenewDocumentCommand" |
		"newcommand" |
		"renewcommand" |
		"def" |
		"let" |
		"gdef" |
		"LetLtxMacro" |
		"DeclareMathOperator*")
	lazy val defApp = defCmd ~ defArg ^^ {case name ~ args => CmdAppTeX(YenTeX(name), args)}
	lazy val defArg = (yen.+ ~ (sqb | bra).*) ^^ {case y ~ args => DocTeX(y ++ args)} | ((sqb | bra).+ ^^ DocTeX)
	lazy val arg: Parser[DocTeX] = ((sqb | bra).* ^^ DocTeX)
	lazy val doc: Parser[DocTeX] = (cmt | esc | tim | env | vrb | cmd | bra | sqb | str | mat).* ^^ DocTeX
	lazy val vrb = vrb1 | vrb2
	lazy val vrb1 = ("\\verb#" ~> "[^#]*".r <~ "#") ^^ {
		case quoted => VrbTeX("#", quoted)
	}
	lazy val vrb2 = ("\\verb|" ~> """[^\|]*""".r <~ "|") ^^ {
		case quoted => VrbTeX("|", quoted)
	}
	lazy val tim = "\\begin{Verbatim}" ~> arg ~ (not("\\end") ~> "[\\S\\s]".r).* <~ "\\end{Verbatim}" ^^ {
		case args ~ lst => LstTeX(args.body.head, lst.mkString)
	}
	def parseTeX(str: String): DocTeX = parseAll(doc, str) match {
		case Success(ast, _) => ast
		case Failure(msg, next) => sys.error(s"${msg}\nat${next.pos}${next.pos.longString}")
		case Error(msg, next) => sys.error(s"${msg}\nat${next.pos}${next.pos.longString}")
	}
	override def skipWhitespace = false
}

object ParamPEGs extends RegexParsers with PackratParsers {
	lazy val m = "m" ^^ ParamM
	lazy val o = "o" ^^ (_ => ParamO(StrTeX("")))
	lazy val O = "O" ~> ("{" ~> str <~ "}") ^^ ParamO
	lazy val str = """[^\}]*""".r ^^ StrTeX
	lazy val params = (m | o | O).*
	def parseAll(str: String): Seq[Param] = parseAll(params, str) match {
		case Success(ast, _) => ast
		case fail: NoSuccess => sys.error(fail.msg)
	}
	override def skipWhitespace = true
}

object Binds {
	val binds = MutableMap[String, DefCmdTeX]()
	def get(name: YenTeX) = binds.get(name.view)
}

object MacroExpandMode {
	var isFinal = false
}

trait TeX {
	def asArg = this.asInstanceOf[ArgTeX]
	def asOpt = this.asInstanceOf[OptTeX]
	def asYen = this.asInstanceOf[YenTeX]

	/**
	 * eval this expression and returns TeX expression
	 */
	def eval: String

	/**
	 * eval this expression and returns body expression
	 *
	 * ex:
	 * [123] returns 123
	 * {123} returns 123
	 */
	def peel = eval

	/**
	 * returns TeX expression without evaluation
	 */
	def view: String

	/**
	 * convert this expression into Markdown AST
	 */
	def toST: middle.ST

	final override def toString() = eval
}

case class CmdAppTeX(name: YenTeX, args: DocTeX = DocTeX()) extends TeX {
	def eval = (Binds.get(name) match {
		case Some(cmd) => cmd.expand(this)
		case None => name.text match {
			case "def" => NewCmdTeX(name, args)
			case "let" => NewCmdTeX(name, args)
			case "gdef" => NewCmdTeX(name, args)
			case "newcommand" => NewCmdTeX(name, args)
			case "renewcommand" => NewCmdTeX(name, args)
			case "NewDocumentCommand" => DocCmdTeX(name, args)
			case "RenewDocumentCommand" if ok => DocCmdTeX(name, args)
			case "LetLtxMacro" => OldCmdTeX(name, args)
			case "DeclareMathOperator*" => NewCmdTeX(name, DocTeX(args.body.take(1) :+ ArgTeX(CmdAppTeX(YenTeX("mathrm"), DocTeX(args.body.drop(1))))))
			case "mathchoice" => args.body.head.asArg.peel
			case "IfValueTF" => args.body(if(args.body.head.asArg.peel.trim.nonEmpty) 1 else 2).asArg.peel
			case name => "%s%s".format(YenTeX(name), args)
		}
	}).toString()
	def view = name.text match {
		case "label" => ""
		case _ => "%s%s".format(name.view, args.view)
	}
	def toST = middle.CmdAppST(name.toST, args.toST)
	def ok = args.body.head.peel match {
		case "\\chapter" => false
		case _ => true
	}
}

trait Param

case class ParamM(partype: String) extends Param
case class ParamO(default: StrTeX) extends Param

abstract class DefCmdTeX(cmd: YenTeX, args: DocTeX) extends TeX {
	def name = args.body.head.peel

	Binds.binds(name) = this

	/**
	 * command body
	 */
	def body = args.body.last match {
		case arg: ArgTeX => arg.view.tail.init
		case yen: YenTeX => yen.view
	}

	/**
	 * command body that can be used as a formatted String
	 */
	val BODY = body.replaceAll("""#(\d)""", """%$1\$s""")	

	/**
	 * command parameters
	 */
	def pars: Seq[Param]

	/**
	 * process command arguments
	 *
	 * @return (used arguments, unused arguments)
	 */
	def passArgs(app: CmdAppTeX): (Seq[TeX], Seq[TeX]) = {
		// all arguments explicitly specified
		if(app.args.body.size >= pars.size) {
			app.args.body.splitAt(pars.size)
		} else {
			val buf = Buffer[TeX]()
			val que = Queue[TeX](app.args.body:_*)
			for(par <- this.pars) par match {
				case ParamM(m) => buf += que.dequeue()
				case ParamO(default) => buf += default
			}
			(buf.toSeq, que.toSeq)
		}
	}

	/**
	 * expand this macro into the specified expression
	 */
	def expand(app: CmdAppTeX): String = {
		var tex = expandOnce(app)
		var exp = Seq[String]().toBuffer
		do {
			exp.prepend(tex)
			tex = TeXPEGs.parseTeX(tex).eval
		} while(!MacroExpandMode.isFinal && !exp.contains(tex))
		tex
	}

	/**
	 * expand this macro into the specified expression
	 */
	def expandOnce(app: CmdAppTeX): String = {
		val (args, rest) = this.passArgs(app)
		val vals = args.map(_.peel)
		this.BODY.format(vals:_*).concat(rest.mkString)
	}

	def eval = ""
	def view = "%s%s".format(cmd.view, args.view)
	def toST = middle.StrST("")
}

case class DocCmdTeX(cmd: YenTeX, args: DocTeX) extends DefCmdTeX(cmd, args) {
	def pars = ParamPEGs.parseAll(args.body(1).asArg.peel)
}

case class NewCmdTeX(cmd: YenTeX, args: DocTeX) extends DefCmdTeX(cmd, args) {
	def pars = Seq.fill(args.body.tail.init.headOption.map(_.peel.toInt).getOrElse(0))(ParamM("M"))
}

case class OldCmdTeX(cmd: YenTeX, args: DocTeX) extends DefCmdTeX(cmd, args) {
	def pars = Seq()
	def renameTo = args.body.tail.head.view.replace("{", "").replace("}", "")
	override def expand(app: CmdAppTeX) = expandOnce(app)
	override def expandOnce(app: CmdAppTeX) = MacroExpandMode.isFinal match {
		case false => app.view
		case true => "%s%s".format(renameTo, app.args)
	}
	override def eval = view
}

case class EnvAppTeX(name: String, args: DocTeX, body: TeX) extends TeX {
	def eval = """\begin{%1$s}%2$s%3$s\end{%1$s}""".format(name, args.eval, body.eval)
	def view = """\begin{%1$s}%2$s%3$s\end{%1$s}""".format(name, args.view, body.view)
	def toST = middle.EnvAppST(middle.StrST(name), args.toST, body.toST, body)
}

case class YenTeX(text: String) extends TeX {
	def eval = (Binds.get(this) match {
		case Some(cmd) if cmd.pars.isEmpty => cmd.expand(CmdAppTeX(this))
		case _ => view
	}).toString()
	def view = """\""".concat(text)
	def toST = middle.YenST(text)
}

case class OptTeX(body: TeX) extends TeX {
	def eval = "[%s]".format(body)
	override def peel = body.eval
	def view = "[%s]".format(body.view)
	def toST = middle.OptST(body.toST)
}

case class ArgTeX(body: TeX) extends TeX {
	def eval = "{%s}".format(body)
	override def peel = body.eval
	def view = "{%s}".format(body.view)
	def toST = middle.ArgST(body.toST)
}

case class StrTeX(text: String) extends TeX {
	def eval = text
	def view = text
	def toST = middle.StrST(text)
}

case class EscTeX(char: String) extends TeX {
	def eval = """\""".concat(char)
	def view = eval
	def toST = middle.EscST(char)
}

case class VrbTeX(del: String, body: String) extends TeX {
	def eval = s"\\verb${del}${body}${del}"
	def view = eval
	def toST = middle.VrbST(body)
}

case class LstTeX(lang: TeX, body: String) extends TeX {
	def eval = s"""\\begin{Verbatim}${lang}${body}\\end{Verbatim}"""
	def view = eval
	def toST = middle.LstST(lang.toST, body)
}

case class MatTeX(body: TeX) extends TeX {
	def eval = s"$$${body.eval.trim}$$"
	def view = s"$$${body.view.trim}$$"
	def toST = middle.MatST(body)
}

case class DocTeX(body: Seq[TeX] = Seq()) extends TeX {
	def eval = body.map(_.eval).mkString
	def view = body.map(_.view).mkString
	def toST = middle.DocST(body.map(_.toST))
}
