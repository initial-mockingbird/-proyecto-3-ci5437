
package Delilah

import Delilah.Algebra.*
import Delilah.Encodings.{EntryFormat, TeamEncoding, generate_matches_for_day,to_dinmacs_str}
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import cats.arrow.Compose
import cats.implicits.*
import cats.syntax.all.*
import io.circe.parser.decode
import Delilah.FromJSON.*
import io.circe.generic.auto.*
import io.circe.syntax.*


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

  "aa" should "aa" in {
    val fp = "./json_inputs/easy1.json"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val f: String => EntryFormat
    = readFile
      >>> decode[EntryFormat]
      >>> (it => it.getOrElse(throw new IllegalStateException("f cannot fail")))
    val r1: EntryFormat = f(fp)
    println(to_dinmacs_str(r1))

  }

}