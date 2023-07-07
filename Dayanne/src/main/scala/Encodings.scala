package Delilah

import Delilah.Algebra.{Algebra, And, Or, VariableTerm}
import cats.arrow.Compose
import cats.syntax.all.*

import java.time.LocalDate
import java.time.LocalTime
import java.time.temporal.ChronoUnit

object Encodings {

case class EntryFormat
  ( tournament_name: String
  , start_date: LocalDate
  , end_date: LocalDate
  , start_time: LocalTime
  , end_time: LocalTime
  , participants: Vector[String]
  )

case class TeamEncoding
  ( local        : Int /* EntryFormat.participants[local] */
  , non_local    : Int /* EntryFormat.participants[non_local] */
  , day          : Int /* EntryFormat.start_date + day */
  //, match_number : Int /* EntryFormat.start_time + 2hours * match_number */
  )


def choose[A](a : Seq[A], k : Int): Seq[Seq[A]] = {

  if (k == 0)      return Seq.empty[Seq[A]]
  if (k == a.size) return a.map((x : A) => Seq(x))
  if (k > a.size)  throw new IllegalArgumentException("k cannot be larger than the iterable")

  val addHead  = choose(a.tail,k-1).map((xs : Seq[A]) => a.head +: xs)
  val skipHead = choose(a.tail,k)

  addHead ++ skipHead
}
type Matchup[A]      = (A,A)
type DailyMatchup[A] = Seq[Matchup[A]]

/* a series of matchups (a,b) is naively legal if its non-empty and there is no team that plays twice */
def is_naively_legal_matchup[A](ms : DailyMatchup[A]) : Boolean
  = ms.size > 0 && 2 * ms.size == ms.flatMap((ab : (A,A)) => Seq.from(List(ab._1,ab._2))).toSet.size

def get_number_of_matches_per_day(start_time: LocalTime, end_time: LocalTime) : Int
  = start_time.until(end_time, ChronoUnit.HOURS).toInt / 2


def embed_matchups[A](ms : DailyMatchup[A]) : Algebra[Matchup[A]]
  = ms.tail.foldRight(VariableTerm(ms.head) : Algebra[Matchup[A]])((matchup,ast) => And(VariableTerm(matchup),ast))

def embed_possible_matchups[A](mss : Seq[DailyMatchup[A]]) : Algebra[Matchup[A]]
  = mss.tail.foldRight(embed_matchups(mss.head))((matchups,ast) => Or(embed_matchups(matchups),ast) )

def make_team_encoding(day: Int)(lv: (Int, Int)): TeamEncoding = TeamEncoding(lv._1, lv._2, day)


def generate_matches_for_day(entryFormat: EntryFormat) : Seq[Algebra[TeamEncoding]]= {
  val matchups = for {
    a <- 0 until entryFormat.participants.length
    b <- 0 until entryFormat.participants.length if a != b
  } yield(a,b)
  val days = 0 until (entryFormat.start_date.until(entryFormat.end_date,ChronoUnit.DAYS).toInt)
  val total_matches  = get_number_of_matches_per_day(entryFormat.start_time,entryFormat.end_time)
  val daily_matchups = choose(matchups,total_matches)
    .filter(is_naively_legal_matchup)
    .map(xs => xs ++ xs.map(_.swap))
  val daily_matchups_ast =  embed_possible_matchups(daily_matchups)

  days.map((d : Int) => daily_matchups_ast.map(make_team_encoding(d)))
}


}


