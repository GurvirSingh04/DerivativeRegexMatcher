// Main Part 3 about Regular Expression Matching
//==============================================

object M3 {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp  // alternatives 
case class SEQs(rs: List[Rexp]) extends Rexp  // sequences
case class STAR(r: Rexp) extends Rexp         // star


//the usual binary choice and binary sequence can be defined 
//in terms of ALTs and SEQs
def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))
def SEQ(r1: Rexp, r2: Rexp) = SEQs(List(r1, r2))

// some convenience for typing regular expressions

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

import scala.language.implicitConversions

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// some examples for the conversion and extension:

// val areg : Rexp = "a" | "b"
//  => ALTs(List(CHAR('a'), CHAR('b')))
//
// val sreg : Rexp = "a" ~ "b"
//  => SEQs(List(CHAR('a'), CHAR('b'))) 
//
// val star_reg : Rexp = ("a" ~ "b").%
//  => STAR(SEQs(List(CHAR('a'), CHAR('b')))) 


def nullable (r: Rexp) : Boolean = {
  r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALTs(rs) => rs.exists(nullable)
    case SEQs(rs) => rs.forall(nullable)
    case STAR(_) => true
  }
}

def der (c: Char, r: Rexp) : Rexp = {
  r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO
    case ALTs(rs) => ALTs(rs.map(der(c, _)))
    case SEQs(List()) => ZERO
    case SEQs(r :: rs) => 
      if (nullable(r)) {
        ALT(SEQs(der(c, r) :: rs), der(c, SEQs(rs)))
      } else {
        SEQs(der(c, r) :: rs)
      }
    case STAR(r) => SEQ(der(c, r), STAR(r))
  }
}
 
def denest(rs: List[Rexp]) : List[Rexp] = {
  rs match {
    case List() => List()
    case ZERO :: rest => denest(rest)
    case ALTs(rs) :: rest => rs ::: denest(rest)
    case r :: rest => r :: denest(rest)
  }
}

def flts(rs: List[Rexp], acc: List[Rexp] = Nil) : List[Rexp] = {
  rs match {
    case List() => acc
    case ZERO :: rest => List(ZERO)
    case ONE :: rest => flts(rest, acc)
    case SEQs(rs) :: rest => flts(rest, acc ::: rs)
    case r :: rest => flts(rest, acc ::: List(r))
  }
}

def ALTs_smart(rs: List[Rexp]) : Rexp = {
  rs match{
    case List() => ZERO
    case List(r) => r
    case _ => ALTs(rs)
  }
}

def SEQs_smart(rs: List[Rexp]) : Rexp = {
  rs match{
    case List() => ONE
    case List(ZERO) => ZERO
    case List(r) => r
    case _ => SEQs(rs)
  }
}

def simp(r: Rexp) : Rexp = {
  r match {
    case ALTs(rs) => ALTs_smart(denest(rs.map(simp)).distinct)
    case SEQs(rs) => SEQs_smart(flts(rs.map(simp)))
    case r => r
  }
}

def ders (s: List[Char], r: Rexp) : Rexp = {
  s match {
    case Nil => r
    case c::cs => ders(cs, simp(der(c, r)))
  }
}

def matcher(r: Rexp, s: String): Boolean = {
  nullable(ders(s.toList, r))
}

def size(r: Rexp): Int = {
  r match {
    case ZERO => 1
    case ONE => 1
    case CHAR(_) => 1
    case ALTs(rs) => 1 + rs.map(size).sum
    case SEQs(rs) => 1 + rs.map(size).sum
    case STAR(r) => 1 + size(r)
  }
}
}
