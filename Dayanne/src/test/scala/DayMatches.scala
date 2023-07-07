
package Delilah

import Delilah.Algebra.*
import Delilah.Encodings.{EntryFormat, TeamEncoding, generate_matches_for_day}
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
    val f  : String => Seq[Algebra[TeamEncoding]]
      = readFile
      >>> decode[EntryFormat]
      >>> (it  => it.getOrElse(throw new IllegalStateException("f cannot fail")))
      >>> generate_matches_for_day
    val r1 : Seq[Set[TeamEncoding]]
      = f(fp).map(it => it.foldl(Set.empty[TeamEncoding])((s : Set[TeamEncoding],te : TeamEncoding) => s + te))
    val day1 = r1.head
    val day2 = r1(1)

    /* it should contain every possible (a,b) for each day */
    assert(day1.contains(TeamEncoding(2,1,0)))
    assert(day1.contains(TeamEncoding(2,0,0)))
    assert(day1.contains(TeamEncoding(1, 2, 0)))
    assert(day1.contains(TeamEncoding(0, 2, 0)))

    assert(day2.contains(TeamEncoding(2, 1, 1)))
    assert(day2.contains(TeamEncoding(2, 0, 1)))
    assert(day2.contains(TeamEncoding(1, 2, 1)))
    assert(day2.contains(TeamEncoding(0, 2, 1)))

  }

}