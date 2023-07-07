package Delilah

import cats.{Foldable, Functor}
import cats.arrow.Compose
import cats.syntax.all.*
import cats.derived.*

object Algebra {
  case class Person(name: String, age: Int)
  /* Boolean Algebra with A-terms */
  sealed trait Algebra[A] derives Foldable, Functor
  case class VariableTerm[A](varT : A)   extends Algebra[A]
  case class Negate[A](neg : Algebra[A]) extends Algebra[A]
  case class And[A](_1 : Algebra[A], _2 : Algebra[A]) extends Algebra[A]
  case class Or[A] (_1 : Algebra[A], _2 : Algebra[A]) extends Algebra[A]

  /*
  implicit val functorAlgebra : Functor[Algebra] = new Functor[Algebra] {
    def map[A,B](fa: Algebra[A])(f : A => B) : Algebra[B] = fa match
      case VariableTerm(varT) => VariableTerm(f(varT))
      case Negate(neg)        => Negate(neg.map(f))
      case And(_1, _2)        => And(_1.map(f),_2.map(f))
      case Or(_1, _2)         => Or(_1.map(f),_2.map(f))
  }
  */


  /* Making it compositional is too much of an overkill for this project */
  def move_negate_inwards[A](term : Algebra[A]) : Algebra[A] =
    term match
      /* ~~p = p */
      case Negate(neg : Negate[A]) =>  move_negate_inwards(neg.neg)
      /* ~(p^q) = ~p v ~q*/
      case Negate(neg : And[A])    => Or(move_negate_inwards(Negate(neg._1)), move_negate_inwards(Negate(neg._2)))
      /* ~(pvq) = ~p ^ ~q*/
      case Negate(neg: Or[A])      => And(move_negate_inwards(Negate(neg._1)), move_negate_inwards(Negate(neg._2)))
      /* ~p = ~p when p is an atom*/
      case Negate(neg : VariableTerm[A]) => Negate(neg)
      /* recursive pass through cases */
      //  if _1 and _2 have their negations pushed, then _1 ^ _2 also have their negations pushed
      case And(_1,_2)              => And(move_negate_inwards(_1), move_negate_inwards(_2))
      //  if _1 and _2 have their negations pushed, then _1 v _2 also have their negations pushed
      case Or(_1,_2)               => Or(move_negate_inwards(_1), move_negate_inwards(_2))
      //  trivial base case
      case VariableTerm(_)         => term

  /* Making it compositional is too much of an overkill for this project */
  /* Assume that term has their negation pushed */
  def distribute_and_over_or[A](term : Algebra[A]) : Algebra[A] =
  term match
    /* (p ^ q) v r = (p v r) ^ (q v r) */
    case Or(_1:And[A],_2) => {
      val p = _1._1
      val q = _1._2
      val r = _2
      val p_or_q = distribute_and_over_or(Or(p,r))
      val q_or_r = distribute_and_over_or(Or(q,r))
      return And(p_or_q,q_or_r)
    }
    /* r v (p ^ q) = (r v p) ^ (r v q) */
    case Or(_1, _2 : And[A]) => {
      val p = _2._1
      val q = _2._2
      val r = _1
      val r_or_p = distribute_and_over_or(Or(r,p))
      val r_or_q = distribute_and_over_or(Or(r,q))
      return And(r_or_p,r_or_q)
    }
    /* If p and q have their ^/v and p !~ (r1 ^ s1) and q !~ (r2,s2) then p v q have their ^/v
    * this is easily seen when you think that `distribute_and_over_or` pushes the `v` inside, and pulls the `^` outside
    * */
    case Or(_1, _2) => Or(distribute_and_over_or(_1), distribute_and_over_or(_2))
    /* recursive pass through cases */
    /* remember that `distribute_and_over_or` pushes `v` inside... but also pulls `^` outside, so
    * even if we end up having: p ^ q ~ p ^ (r ^ s), we have nothing to push anymore.
    * */
    case And(_1, _2) => And(distribute_and_over_or(_1), distribute_and_over_or(_2))
    /* remember that we assume that all negations are pushed, thus ~p always unifies with ~Variable*/
    case Negate(neg : VariableTerm[A]) => Negate(distribute_and_over_or(neg))
    /*dumb base case*/
    case VariableTerm(_) => term

  def to_CNF[A](t : Algebra[A]) : Algebra[A] = (move_negate_inwards[A] >>> distribute_and_over_or[A])(t)


}
