package com.example.regexp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

private[regexp] object AST {

  sealed abstract class Node
  case class Literal(c: Char) extends Node
  case class Catenation(e1: Node, e2: Node) extends Node
  case class Alternation(e1: Node, e2: Node) extends Node
  case class Question(e: Node) extends Node
  case class Star(e: Node) extends Node
  case class Plus(e: Node) extends Node

  def parsePostfix(postfix: String): Node = {
    val l = postfix.toList.foldLeft(List[Node]())((e, c) =>
      (e, c) match {
        case (e2 :: e1 :: ex, '.') => Catenation(e1, e2) :: ex
        case (e2 :: e1 :: ex, '|') => Alternation(e1, e2) :: ex
        case (e1 :: ex, '?')       => Question(e1) :: ex
        case (e1 :: ex, '*')       => Star(e1) :: ex
        case (e1 :: ex, '+')       => Plus(e1) :: ex
        case (_, _)                => Literal(c) :: e
      }
    )
    assert(l.tail == Nil)
    l.head
  }

  val parse = InfixParser.parse _
}

private object InfixParser extends Parsers {
  type Elem = Char;

  implicit def char(c: Char): Parser[Char] = new Parser[Char] {
    def apply(in: Input) = {
      val found = in.first
      if (c == found)
        Success(c, in.rest)
      else
        Failure("'" + c + "' expected but " + found + " found", in.rest)
    }
  }

  // このBNFは https://9p.io/magic/man2html/6/regexp を参考に実装した。

  def e0: Parser[AST.Node] = (
    e1 ~ '|' ~ e0 ^^ { case e1 ~ _ ~ e0 => AST.Alternation(e1, e0) }
      | e1
  )
  def e1: Parser[AST.Node] = (
    e2 ~ e1 ^^ { case e2 ~ e1 => AST.Catenation(e2, e1) }
      | e2
  )
  def REP: Parser[Char] = char('*') | '+' | '?'
  def e2: Parser[AST.Node] = (
    e3 ~ REP ^^ {
      case e3 ~ '*' => AST.Star(e3)
      case e3 ~ '+' => AST.Plus(e3)
      case e3 ~ '?' => AST.Question(e3)
      case _        => throw new Exception("must not reach here")
    }
      | e3
  )
  def e3: Parser[AST.Node] = (
    literal ^^ { case c => AST.Literal(c) }
      | '(' ~ e0 ~ ')' ^^ { case _ ~ e0 ~ _ => e0 }
  )
  def literal: Parser[Char] =
    elem("literal", (!Set('+', '*', '?', '|', '(', ')').contains(_)))

  def parse(pattern: String): AST.Node = {
    phrase(e0)(new CharSequenceReader(pattern)).get
  }
}
