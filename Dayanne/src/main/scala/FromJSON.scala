package Delilah

import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import cats.effect.IO
import cats.effect.std.Console

import java.time.LocalDate
import java.time.LocalTime
import scala.util.Try
import cats.effect.{IO, IOApp}
import cats.arrow.Compose
import cats.syntax.all.*
import io.circe.generic.auto.*
import io.circe.syntax.*

import java.time.format.DateTimeFormatter


object FromJSON{

  val stripT = (s: String) => s // s.dropWhile(c => c == 'T' || c == 't')

  implicit val decodeLocalDate: Decoder[LocalDate] = Decoder.decodeString.emapTry {
    stripT >>> (it => LocalDate.parse(it,DateTimeFormatter.ISO_DATE )) >>> (s => Try[LocalDate](s))
  }

  implicit val decodeLocalTime: Decoder[LocalTime] = Decoder.decodeString.emapTry {
    stripT >>> (it => LocalTime.parse(it,DateTimeFormatter.ISO_TIME)) >>> (s => Try[LocalTime](s))
  }

  
}

