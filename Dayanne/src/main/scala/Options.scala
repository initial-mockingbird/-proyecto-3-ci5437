import Delilah.Encodings.EntryFormat
import cats.syntax.either.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.yaml
import Delilah.Algebra.*
import Delilah.Encodings.*
import Delilah.Tournament.*

import scala.io.Source
import cats.arrow.Compose
import cats.implicits.*
import cats.syntax.all.*
import io.circe.parser.decode
import Delilah.FromJSON.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import net.fortuna.ical4j.data.CalendarOutputter

import java.io.{File, FileOutputStream, FileWriter}
import scala.io.Source
import scala.sys.process.*
import net.fortuna.ical4j.model.Calendar
object Options {

  case class Options
  (entryPoint: String
   , glucoseExecutable: String
   , glucoseInput: String
   , glucoseOutput: String
   , output: String
  )

  def runner() = {
    val fp = "./options.yaml"
    val readFile = (fp: String) => Source.fromFile(fp).mkString
    val options = (readFile >>> yaml.parser.parse >>> (it => it.toOption.get.toString) >>> decode[Options])(fp).toOption.get
    val entryFormat = (readFile >>> decode[EntryFormat])(options.entryPoint).toOption.get
    val glucoseInputW = new FileWriter(new File(options.glucoseInput))
    val dinmacs = to_dinmacs_str(entryFormat)
    glucoseInputW.write(dinmacs)
    glucoseInputW.close()
    println(options.glucoseExecutable + " " + options.glucoseInput + " " + options.glucoseOutput)
    (options.glucoseExecutable + " " + options.glucoseInput + " " + options.glucoseOutput).!!
    val dinmacsSol = (readFile >>> (it => it.split(" ").dropRight(1).map(s => s.toInt).filter(s => s > 0)))(options.glucoseOutput)
    val calendarSol = translate_team_vars_into_itinerary(entryFormat)(dinmacsSol.map(translate_dinmacs_var(entryFormat)))
    calendarSol.foreach {
      println
    }
    val calendar= export_calendar(entryFormat.tournament_name, calendarSol)
    val fout : FileOutputStream = new FileOutputStream(options.output);
    val outputter : CalendarOutputter = new CalendarOutputter();
    outputter.output(calendar, fout);
  }
}