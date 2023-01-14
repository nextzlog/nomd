package engine

import scala.io.Source
import scala.util.Using
import decode.Counter
import encode.TeXPEGs
import encode.MacroExpandMode

object NoMD {
	def main(args: Array[String]): Unit = println(process(args))
	def process(args: Array[String]): String = {
		val raws = args.map(path => Source.fromFile(path).mkString)
		val head = Source.fromResource("header.tex").mkString
		val docs = raws.dropRight(1) ++ Seq(head) ++ raws.lastOption.toSeq
		val tree = docs.map(text => TeXPEGs.parseTeX(text).eval)
		MacroExpandMode.isFinal = true
		val fins = tree.map(text => TeXPEGs.parseTeX(text).eval)
		val text = fins.map(text => TeXPEGs.parseTeX(text).toST.toMD)
		// 1st scan (unlabeled)
		text.lastOption.map(md => md.str(md)).getOrElse("")
		Counter.clear()
		// 2nd scan
		text.takeRight(2).map(md => md.str(md).trim).mkString("\n")
	}
}
