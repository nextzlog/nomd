package decode

import encode.TeX
import scala.collection.mutable.{Map => MutableMap}

trait MD {
	def str(scope: MD): String
	def lab(scope: MD): Seq[String]
	def cap(scope: MD): Seq[String]
	def mid: Boolean = false
}

abstract class LeafMD extends MD {
	override def lab(scope: MD): Seq[String] = Seq()
	override def cap(scope: MD): Seq[String] = Seq()
}

abstract class TreeMD(children: MD*) extends MD {
	override def mid = children.exists(_.mid)
	def lab(scope: MD) = children.map(_.lab(scope)).flatten
	def cap(scope: MD) = children.map(_.cap(scope)).flatten
}

abstract class CmdBodyMD(args: DocMD) extends TreeMD(args)

abstract class EnvBodyMD(body: MD) extends TreeMD(body)

case class YenMD(text: String) extends LeafMD {
	override def str(scope: MD) = text
}

case class OptMD(body: MD) extends TreeMD(body) {
	override def str(scope: MD) = body.str(scope)
}

case class ArgMD(body: MD) extends TreeMD(body) {
	override def str(scope: MD) = body.str(scope)
}

case class StrMD(text: String) extends LeafMD {
	override def str(scope: MD) = text.replace('~', ' ').replace("|", "\\|")
}

case class EscMD(char: String) extends LeafMD {
	override def str(scope: MD) = char match {
		case "\\" => "\n"
		case _ => char
	}
}

object EmptyMD extends LeafMD {
	override def str(scope: MD) = ""
}

case class VrbMD(body: String) extends LeafMD {
	override def str(scope: MD) = "`%s`".format(body)
}

case class LstMD(lang: MD, body: String) extends LeafMD {
	override def str(scope: MD) = s"```${lang.str(scope).toLowerCase}${body}```"
}

case class MatMD(body: TeX) extends LeafMD {
	override def str(scope: MD) = " $%s$ ".format(body.view.replace("|", " \\vert ").trim)
}

case class DocMD(body: Seq[MD]) extends TreeMD(body: _*) {
	override def str(scope: MD) = body.map(_.str(scope)).mkString
}

object Counter {
	val counter = MutableMap[String, Int]().withDefaultValue(0)
	val labeled = MutableMap[String, String]()
	def next(label: String) = {
		counter(label.split(":", 2).head) += 1
		counter(label.split(":", 2).head)
	}
	def reset(label: String) = {
		counter(label.split(":", 2).head) = 0
	}
	def now(label: String) = {
		counter(label.split(":", 2).head)
	}
	def clear() = counter.clear()
}

case class Label(args: DocMD) extends CmdBodyMD(args) {
	override def lab(scope: MD) = Seq(args.body.head.str(scope))
	override def str(scope: MD) = ""
}

case class Ref(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = Counter.labeled.getOrElse(Label(args).lab(scope).head, "?")
}

case class Caption(args: DocMD) extends CmdBodyMD(args) {
	override def cap(scope: MD) = Seq(str(scope))
	override def str(scope: MD) = {
		val chap = Counter.now("chap")
		val fig = Counter.now("fig")
		val tab = Counter.now("tab")
		scope match {
			case Figure(_,_,_) => s"Fig. $chap.$fig " + args.body.head.str(scope)
			case Table(_,_,_) => s"Table $chap.$tab " + args.body.head.str(scope)
		}
	}
}

case class IncludeGraphics(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		val sub = Counter.next("subfig")
		val path = args.body.last.str(scope).replaceAll(".eps$", ".png")
		val cap = scope.cap(scope).headOption.getOrElse("")
		val paren = scope match {
			case SubFloat(_) => {
				scope.lab(scope).headOption.foreach(lab => Counter.labeled(lab) = s"($sub)")
				s"\n\n($sub) $cap"
			}
			case _ => ""
		}
		"![%s](/%s)%s\n".format(path, path, paren)
	}
}

case class SubFloat(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = args.body.last.str(this)
	override def cap(scope: MD) = Seq(args.body.head.str(this))
}

case class Chapter(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		Counter.reset("eq")
		Counter.reset("fig")
		Counter.reset("sect")
		Counter.reset("tab")
		val chap = Counter.next("chap")
		this.lab(scope).headOption.foreach(lab => Counter.labeled(lab) = s"$chap")
		s"## $chap ${args.str(scope)}"
	}
}

case class Section(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		val chap = Counter.now("chap")
		val sect = Counter.next(this.lab(scope).headOption.getOrElse("sect"))
		this.lab(scope).headOption.foreach(lab => Counter.labeled(lab) = s"$chap.$sect")
		s"### $chap.$sect ${args.str(scope)}"
	}
}

case class HRef(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"[${args.body.tail.head.str(scope)}](${args.body.head.str(scope)})"
}

case class URL(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"[${args.body.head.str(scope)}](${args.body.head.str(scope)})"
}

case class TextBf(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"**${args.str(scope)}**"
}

case class TextIt(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"*${args.str(scope)}*"
}

case class Title(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"${args.str(scope)}\n===\n"
}

case class TextTt(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = args.str(scope)
}

case class MathChoice(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = args.body.head.str(scope)
}

case class OutputNothingCmd(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = ""
}

case class OutputNothingEnv(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = ""
}

class MidRule(args: DocMD) extends OutputNothingCmd(args) {
	override def mid = true
}

case class Document(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = body.str(this)
}

case class Equation(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = {
		val chap = Counter.now("chap")
		val idx = Counter.next(this.lab(scope).headOption.getOrElse("eq"))
		this.lab(scope).headOption.foreach(lab => Counter.labeled(lab) = s"$chap.$idx")
		s"$$$$${tex.view.trim} \\qquad($chap.$idx)$$$$"
	}
}

case class AlignAt(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	import encode._
	override def str(scope: MD) = Equation(args, body, EnvAppTeX("alignedat", DocTeX(Seq(ArgTeX(StrTeX(args.str(scope))))), tex)).str(scope)
}

case class Figure(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = {
		val chap = Counter.now("chap")
		val fig = Counter.next(this.lab(scope).headOption.getOrElse("fig"))
		this.lab(scope).lastOption.foreach(lab => Counter.labeled(lab) = s"$chap.$fig")
		Counter.reset("subfig")
		body.str(this).trim
	}
}

case class Table(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = {
		val chap = Counter.now("chap")
		val tab = Counter.next(this.lab(scope).headOption.getOrElse("tab"))
		this.lab(scope).headOption.foreach(lab => Counter.labeled(lab) = s"$chap.$tab")
		Counter.reset("subtab")
		body.str(this).trim
	}
}

case class Tabular(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = {
		val chap = Counter.now("chap")
		val tab = Counter.now("tab")
		val sub = Counter.next("subtab")
		val ncol = args.body.head.str(scope).count(_.toChar.isLetter)
		val rows = body.str(scope).replace(" & ", " | ").replace("\\\\", "").trim.linesIterator.toSeq
		val head = Seq(if(body.mid) rows.head else Seq.fill(ncol)("-").mkString("|"))
		val rule = Seq.fill(ncol)("---").mkString("|")
		val tail = if(body.mid) rows.tail else rows
		val cap = scope.cap(scope).headOption.getOrElse("")
		val data = (head :+ rule) ++ tail.filterNot(_.trim.isEmpty)
		val paren = scope match {
			case SubFloat(_) => {
				this.lab(scope).headOption.foreach(lab => Counter.labeled(lab) = s"$chap.$tab($sub)")
				s"($sub) $cap\n"
			}
			case _ => ""
		}
		(s"$paren" +: data.map("|%s|".format(_))).mkString("\n")
	}
}

case class MiniPage(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = body.str(scope)
}
