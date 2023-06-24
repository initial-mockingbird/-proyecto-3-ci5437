package Delilah

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source
import cats.arrow.Compose
import cats.implicits._
import cats.syntax.all._
import io.circe.parser.decode
import Delilah.FromJSON._
import io.circe.generic.auto._, io.circe.syntax._

class JsonDecoderSpec extends AnyFlatSpec {

  "json inputs" should "all parse successfully" in {
    val fp = "./json_inputs/test_list.json"
    val readFile = (fp : String) => Source.fromFile(fp).mkString
    val f = readFile >>> decode[Vector[EntryFormat]] >>> (it => it.isRight)
    assert(f(fp))
  }
}
