
package Delilah

import Delilah.Algebra.*
import Delilah.Encodings.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import cats.arrow.Compose
import cats.implicits.*
import cats.syntax.all.*
import io.circe.parser.decode
import Delilah.FromJSON.*
import io.circe.generic.auto.*
import io.circe.syntax.*

import java.io.{File, FileWriter}


class DayMatches extends AnyFlatSpec {

  "just a unit" should "test" in {
    val fp       = "./json_inputs/easy1.json"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val f  : String => Algebra[TeamEncoding]
      = readFile
      >>> decode[EntryFormat]
      >>> (it  => it.getOrElse(throw new IllegalStateException("f cannot fail")))
      >>> generate_matches_for_day
    val r1 : Set[(Int,Int,Int)]
      = f(fp)
        .foldl(Set.empty[TeamEncoding])((s : Set[TeamEncoding],te : TeamEncoding) => s + te)
        .map(it => (it.local,it.non_local,it.day))
    val day1 = r1
    val day2 = r1

    /* it should contain every possible (a,b) for each day */
    assert(day1.contains((2,1,0)))
    assert(day1.contains((2,0,0)))
    assert(day1.contains((1, 2, 0)))
    assert(day1.contains((0, 2, 0)))

    assert(day2.contains((2, 1, 1)))
    assert(day2.contains((2, 0, 1)))
    assert(day2.contains((1, 2, 1)))
    assert(day2.contains((0, 2, 1)))

  }

  "easy1 twice in a row restiction" should "aa" in {
    val fp = "./json_inputs/easy1.json"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val f: String => EntryFormat
    = readFile
      >>> decode[EntryFormat]
      >>> (it => it.getOrElse(throw new IllegalStateException("f cannot fail")))
    val r1: EntryFormat = f(fp)
    val res = (generate_twice_in_a_row_restriction >>> flatten_CNF_And)(r1)
    println(res)
    /* returns:
      List(
      Or(Negate(VariableTerm(TeamVar(0,0,0,true))),Negate(VariableTerm(TeamVar(0,1,0,true)))),
      Or(Negate(VariableTerm(TeamVar(0,0,0,false))),Negate(VariableTerm(TeamVar(0,1,0,false)))),
      Or(Negate(VariableTerm(TeamVar(0,0,1,true))),Negate(VariableTerm(TeamVar(0,1,1,true)))),
      Or(Negate(VariableTerm(TeamVar(0,0,1,false))),Negate(VariableTerm(TeamVar(0,1,1,false)))),
      Or(Negate(VariableTerm(TeamVar(1,0,0,true))),Negate(VariableTerm(TeamVar(1,1,0,true)))),
      Or(Negate(VariableTerm(TeamVar(1,0,0,false))),Negate(VariableTerm(TeamVar(1,1,0,false)))),
      Or(Negate(VariableTerm(TeamVar(1,0,1,true))),Negate(VariableTerm(TeamVar(1,1,1,true)))),
      Or(Negate(VariableTerm(TeamVar(1,0,1,false))),Negate(VariableTerm(TeamVar(1,1,1,false)))),
      Or(Negate(VariableTerm(TeamVar(2,0,0,true))),Negate(VariableTerm(TeamVar(2,1,0,true)))),
      Or(Negate(VariableTerm(TeamVar(2,0,0,false))),Negate(VariableTerm(TeamVar(2,1,0,false)))),
      Or(Negate(VariableTerm(TeamVar(2,0,1,true))),Negate(VariableTerm(TeamVar(2,1,1,true)))),
      Or(Negate(VariableTerm(TeamVar(2,0,1,false))),Negate(VariableTerm(TeamVar(2,1,1,false))))
    )
    Which checks out
    * */

  }

  "easy1 match everybody restiction" should "aa" in {
    val fp = "./json_inputs/easy1.json"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val f: String => EntryFormat
    = readFile
      >>> decode[EntryFormat]
      >>> (it => it.getOrElse(throw new IllegalStateException("f cannot fail")))
    val r1: EntryFormat = f(fp)
    val res = (generate_match_everybody_restriction  >>> toPrettyString)(r1)
    println(res)
    /* returns:

    Which checks out
    * */
  }

  "aa" should "aa" in {
    val fp = "./json_inputs/easy1.json"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val f: String => EntryFormat
    = readFile
      >>> decode[EntryFormat]
      >>> (it => it.getOrElse(throw new IllegalStateException("f cannot fail")))
    val r1: EntryFormat = f(fp)
    val res = to_dinmacs_str(r1)
    val fileWriter = new FileWriter(new File("./dinmacs/easy1.txt"))
    fileWriter.write(res)
    fileWriter.close()

  }

}