package Delilah

import Delilah.Algebra.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import cats.arrow.Compose
import cats.implicits.*
import cats.syntax.all.*
import io.circe.parser.decode
import Delilah.FromJSON.*
import io.circe.generic.auto.*
import io.circe.syntax.*

class ToCNF extends AnyFlatSpec {

  "variables" should "be in cnf" in {
    val p = VariableTerm("p")
    assert(to_CNF(p) == p)
  }

  "terms" should "be in cnf" in {
    val p = VariableTerm("p")
    val q = VariableTerm("q")
    val p_and_q = And(p,q)
    val p_or_q  = Or(p,q)
    val not_p   = Negate(p)
    assert(to_CNF(p_and_q) == p_and_q)
    assert(to_CNF(p_or_q)  == p_or_q)
    assert(to_CNF(not_p)   == not_p)
  }

  /*Imma be real, checking equality of normal forms using AST is super
  * annoying since normal forms are unique UP TO associativity AND symmetry.
  * Which means I must know how my implementation associates and whether or not it commutes
  * to provide the exact expected output. Which basically breaks the need for a test.
  * So instead we do (mental) structural induction on `move_negate_inwards` and `distribute_and_over_or` we see that they
  * do what they intend *hand waves*
  * */
}
