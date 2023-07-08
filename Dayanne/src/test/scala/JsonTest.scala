package Delilah

import Delilah.Encodings.EntryFormat
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import cats.arrow.Compose
import cats.implicits.*
import cats.syntax.all.*
import io.circe.parser.decode
import Delilah.FromJSON.*
import io.circe.generic.auto.*
import io.circe.syntax.*

class JsonDecoderSpec extends AnyFlatSpec {

  "json inputs" should "all parse successfully" in {
    val fp = "./json_inputs/test_list.json"
    val readFile = (fp : String) => Source.fromFile(fp).mkString
    val f = readFile >>> decode[Vector[EntryFormat]] >>> (it => it.isRight)
    assert(f(fp))
  }

}
