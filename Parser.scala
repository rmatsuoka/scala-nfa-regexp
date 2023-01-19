package com.example.regexp

import scala.util.parsing.combinator._

abstract class AST
case class Literal(c: Char) extends AST
case class Catenation(e1: AST, e2: AST) extends AST
case class Alternation(e1: AST, e2: AST) extends AST
case class Question(e: AST) extends AST
case class Star(e: AST) extends AST
case class Plus(e: AST) extends AST

object AST {
  def parsePostfix(postfix: String): AST = {
    val l = postfix.toList.foldLeft(List[AST]())((e, c) =>
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

  def parse(pattern: String): AST = InfixParser.mustParse(pattern)
}

private object InfixParser extends RegexParsers {
  def e0: Parser[AST] = (
    e1 ~ "|" ~ e0 ^^ { case e1 ~ _ ~ e0 => Alternation(e0, e1) }
      | e1
  )
  def e1: Parser[AST] = (
    e2 ~ e1 ^^ { case e2 ~ e1 => Catenation(e2, e1) }
      | e2
  )
  def REP: Parser[String] = "*" | "+" | "?"
  def e2: Parser[AST] = (
    e3 ~ REP ^^ {
      case e3 ~ "*" => Star(e3)
      case e3 ~ "+" => Plus(e3)
      case e3 ~ "?" => Question(e3)
      case _        => throw new Exception("must not reach here")
    }
      | e3
  )
  def e3: Parser[AST] = (
    literal ^^ { case c => Literal(c) }
      | "(" ~ e0 ~ ")" ^^ { case _ ~ e0 ~ _ => e0 }
  )
  def literal: Parser[Char] = """[A-Za-z0-9]""".r ^^ (_.charAt(0))

  def mustParse(pattern: String): AST = {
    parse(e0, pattern) match {
      case Success(result, _) => result
      case Error(msg, next)   => throw new Exception(msg)
      case Failure(msg, next) => throw new Exception(msg)
    }
  }
}
