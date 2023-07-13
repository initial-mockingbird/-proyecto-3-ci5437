package Delilah

import Delilah.Algebra.*
import Delilah.Tournament.TournamentGame
import cats.arrow.Compose
import cats.syntax.all.*
import scala.collection.mutable
import java.time.LocalDate
import java.time.LocalTime
import java.time.temporal.ChronoUnit

object Encodings {

/***********/
/** TYPES **/
/***********/

/* A matchup is just a tuple of A vs A' */
type Matchup[A]      = (A,A,Int)
/* The matchups for the day is a collection of matchups. We use seq in order to concatenate */
type DailyMatchup[A] = Seq[Matchup[A]]

/* Represents the entry format for the data, essentially where the json data will be read*/
case class EntryFormat
  ( tournament_name: String
  , start_date: LocalDate
  , end_date: LocalDate
  , start_time: LocalTime
  , end_time: LocalTime
  , participants: Vector[String]
  )

// Dinmacs variables are integers, so that
type DinMacsVar = Int

/* the only variables we will have in our model are TeamVars. Everything else will either be a boolean expression
* containing these variables or compiled to one */
case class TeamVar
  ( team     : Int
  , day      : Int
  , match_no : Int
  , isLocal  : Boolean
  )


/* Intermediate representation. We model encounters as TeamEncodings and then we map these to TeamVars */
case class TeamEncoding
  ( local        : Int /* EntryFormat.participants[local] */
  , non_local    : Int /* EntryFormat.participants[non_local] */
  , day          : Int /* EntryFormat.start_date + day */
  , match_no     : Int /* EntryFormat.start_time + 2hours * match_number */
  )

  /*************/
  /** Parsers **/
  /*************/

// We choose these type signature since we can always functor-map over the algebra the TeamVar into DinMacsVar
// and then just translate the algebra
def translate_team_encoding(teamEncoding: TeamEncoding) : Algebra[TeamVar]
  = VariableTerm(TeamVar(teamEncoding.local,teamEncoding.day,teamEncoding.match_no,true))
  & VariableTerm(TeamVar(teamEncoding.non_local,teamEncoding.day,teamEncoding.match_no,false))

/*
* Let
* TeamVar
*  ( team     : N
*  , day      : M
*  , match_no : R
*  , isLocal  : 2
*  )
* Then we define the mapping as:
*
* f(team,day,match_no,isLocal) -> team * |M| * |R| * 2 + day * |R| * 2 + match_no * 2 + isLocal + 1
*
* We add the + 1 so the inyection is from (1,...) instead of (0,...)
* Getting the inverse of this is simple since we can retrieve each component as:
* (team * |M| * |R| * 2 + day * |R| * 2 + match_no * 2 + isLocal) % 2 = isLocal
* (team * |M| * |R| * 2 + day * |R| * 2 + match_no * 2 + isLocal) / 2 % |R|
*   = (team * |M| * |R|  + day * |R| + match_no) % |R| = match_no
* (team * |M| * |R|  + day * |R| + match_no) / R % N = (team * |M|  + day) % N = day
* (team * |M|  + day) / |M| % |N| = team % |N| = team
* */
def translate_team_var(entryFormat: EntryFormat)(teamVar: TeamVar) : DinMacsVar = {
  val M = entryFormat.start_date.until(entryFormat.end_date, ChronoUnit.DAYS).toInt + 1
  val R = get_number_of_matches_per_day(entryFormat.start_time, entryFormat.end_time)
  val team = teamVar.team
  val day  = teamVar.day
  val match_no = teamVar.match_no
  val isLocal  = if (teamVar.isLocal) 0 else 1
  team * M * R * 2 + day * R * 2 + match_no * 2 + isLocal + 1
}

  def translate_dinmacs_var(entryFormat: EntryFormat)(dinMacsVar: DinMacsVar) : TeamVar = {
    val M = entryFormat.start_date.until(entryFormat.end_date, ChronoUnit.DAYS).toInt + 1
    val R = get_number_of_matches_per_day(entryFormat.start_time, entryFormat.end_time)

    // remember that we get a mapping from (1..), so we move it to (0...)
    var current     = dinMacsVar - 1
    val isLocalP    = (current % 2) == 0
    current         = current / 2
    val matchup_noP = current % R
    current         = current / R
    val dayP        = current % M
    val teamP       = current / M
    TeamVar(teamP,dayP,matchup_noP,isLocalP)
  }

  def translate_team_vars_into_itinerary(entryFormat: EntryFormat)(vars : Seq[TeamVar]) : Seq[TournamentGame] = {

    vars.foldLeft(mutable.Map.empty.withDefaultValue(Seq.empty))((acc : mutable.Map[(LocalDate,LocalTime),Seq[(String,Boolean)]],tv : TeamVar) =>
      val hour = entryFormat.start_time.plusHours(tv.match_no*2)
      val day  = entryFormat.start_date.plusDays(tv.day)
      acc((day,hour)) = (entryFormat.participants(tv.team),tv.isLocal) +: acc((day,hour))
      acc
      ).foldLeft(Seq.empty)((acc,kv) => {
        val k       = kv._1
        val v       = kv._2
        val local   = v.find((p) => p._2).get._1
        val visitor = v.find((p) => !p._2).get._1
        TournamentGame(k._1,k._2,local,visitor) +: acc
    })
  }

/* Return all the possible ways we can choose k elements of a sequence */
def choose[A](a : Seq[A], k : Int): Seq[Seq[A]] = {

  if (k == 0)      return Seq.empty[Seq[A]]
  if (k == a.size) return Seq(a)
  if (k > a.size)  throw new IllegalArgumentException("k cannot be larger than the iterable")

  val addHead  = choose(a.tail,k-1).map((xs : Seq[A]) => a.head +: xs)
  val skipHead = choose(a.tail,k)


  addHead ++ skipHead
}

/* a series of matchups (a,b) is naively legal if its non-empty and there is no team that plays twice */
def is_naively_legal_matchup[A](ms : DailyMatchup[A]) : Boolean
  = ms.size > 0 && 2 * ms.size == ms.flatMap((ab : (A,A,Int)) => Seq.from(List(ab._1,ab._2))).toSet.size

/* Given a start and end time, returns how many matchups can be done per day */
def get_number_of_matches_per_day(start_time: LocalTime, end_time: LocalTime) : Int
  = start_time.until(end_time, ChronoUnit.HOURS).toInt / 2

/* Given a sequence of matchups for the day, construct the correspondent boolean terms. That is, it joins
* them using `And` (since they ALL have to play that day) */
def embed_matchups[A](ms : DailyMatchup[A]) : Algebra[Matchup[A]]
  = ms.drop(1).foldRight(VariableTerm(ms.head) : Algebra[Matchup[A]])((matchup,ast) => ast & VariableTerm(matchup))

/* Given a sequence of all possible matchups, builds up the boolean terms by:
* - Building the boolean terms for each posible day config
* - Joining them using an Or (since at least one of them has to be true)
*  */
def embed_possible_matchups[A](mss : Seq[DailyMatchup[A]]) : Algebra[Matchup[A]]
  = mss.drop(1).foldRight(embed_matchups(mss.head))((matchups,ast) => ast | embed_matchups(matchups) )

/* Just an utility function */
def make_team_encoding(day: Int)(lv: (Int, Int, Int)): TeamEncoding = TeamEncoding(lv._1, lv._2, day, lv._3)


def generate_matches_for_day(entryFormat: EntryFormat) : Algebra[TeamEncoding] = {
  /* We encode participants not by their names, but rather by their position in the participants vector */
  val matchups = for {
    a <- 0 until entryFormat.participants.length
    b <- 0 until entryFormat.participants.length if a != b
  } yield(a,b)
  val days = 0 until entryFormat.start_date.until(entryFormat.end_date,ChronoUnit.DAYS).toInt + 1
  val total_matches  = get_number_of_matches_per_day(entryFormat.start_time,entryFormat.end_time)
  /* We need both (a,b) and (b,a) pairs, so since n choose k just gives us (a,b) we add the swaps manually */
  val daily_matchups = matchups.combinations(total_matches)
    .map(_.zipWithIndex.map(it => (it._1._1,it._1._2,it._2) ))
    .filter(is_naively_legal_matchup)
    .toSeq
    //.map(xs => xs ++ xs.map(it => (it._2,it._1,it._3)))
  val daily_matchups_ast =  embed_possible_matchups(daily_matchups)
  val aux = toPrettyString(daily_matchups_ast)
  val rs = days.map((d : Int) => daily_matchups_ast.map(make_team_encoding(d)))

  rs.drop(1).foldLeft(rs.head)((acc,b) => acc & b)

}

def generate_twice_in_a_row_restriction(entryFormat: EntryFormat) : Algebra[TeamVar] = {

    val days = 0 until (entryFormat.start_date.until(entryFormat.end_date,ChronoUnit.DAYS).toInt)  + 1
    val total_matches  = get_number_of_matches_per_day(entryFormat.start_time,entryFormat.end_time)
    val restriction_gen = for {
      day <- days
      team <- 0 until entryFormat.participants.length
      matchup_number_today    <- 0 until total_matches
      matchup_number_tomorrow <- 0 until total_matches
    } // If the team played as local on a `day`, then it cannot play as local in the next day
      // If the team played as visitor on a `day`, then it cannot play as visitor in the next day
      yield (VariableTerm(TeamVar(team,day,matchup_number_today,true))  |> ~VariableTerm(TeamVar(team,day+1,matchup_number_tomorrow,true)))
      &     (VariableTerm(TeamVar(team,day,matchup_number_today,false)) |> ~VariableTerm(TeamVar(team,day+1,matchup_number_tomorrow,false)))


    restriction_gen.drop(1).foldLeft(restriction_gen.head)((ast,cond) => ast & cond)
}

def generate_match_everybody_restriction(entryFormat: EntryFormat) : Algebra[TeamVar] = {
  val day_upper_limit = entryFormat.start_date.until(entryFormat.end_date, ChronoUnit.DAYS).toInt
  val days = 0 until day_upper_limit + 1
  val total_matches  = get_number_of_matches_per_day(entryFormat.start_time,entryFormat.end_time)

  // my brain melted trying to use iterators + for comprehension for this one, and I didnt want to bother
  // checking if iterators are compatible with Lists in order to use monad comprehension instead.
  var clauses_seq : Seq[Algebra[TeamVar]] = Seq.empty
  for (a<- 0 until entryFormat.participants.length; b <- a+1 until entryFormat.participants.length)
  {
    // at this point, i say: fuck nomenclature, if I'm going imperative I'm naming things like god actually intended
    // using the glorious camelCase
    var acc: Seq[Algebra[TeamVar]] = Seq.empty
    for (dayA <- days; matchA <- 0 until total_matches) {
      val matchAL = VariableTerm(TeamVar(a,dayA,matchA,true)) & VariableTerm(TeamVar(b,dayA,matchA,false))
      val matchAV = VariableTerm(TeamVar(a,dayA,matchA,false)) & VariableTerm(TeamVar(b,dayA,matchA,true))
      for (dayB <- dayA + 1 until day_upper_limit; matchB <- 0 until total_matches) {
        val matchBL = VariableTerm(TeamVar(a,dayB,matchB,false)) & VariableTerm(TeamVar(b,dayB,matchB,true))
        val matchBV = VariableTerm(TeamVar(a,dayB,matchB,true)) & VariableTerm(TeamVar(b,dayB,matchB,false))
        // this generates something like:
        // (P_a_dayA_matchA_local ^ P_b_dayB_matchB_visitor) | (P_a_dayA_matchA_vistor ^ P_b_dayB_matchB_local)
        // that is, this for generates all possible ways that two contestants can battle each other: one as local
        // and one as visitor for each possible day/matchup configuration.
        acc = ((matchAL & matchBL) | (matchAV & matchBV) )  +: acc
      }
    }
    // we need to combine the configurations using `Or`, since it suffices that one is true for two contestants
    // to fight each other as local and then visitor.
    clauses_seq = acc.drop(1).foldLeft(acc.head)((acc,clause) => acc | clause) +: clauses_seq
  }

  // we link with `And` since we need
  clauses_seq.drop(1).foldLeft(clauses_seq.head)((acc,t) => acc & t)
}



def to_dinmacs_str(entryFormat: EntryFormat) : String = {
  val trans_team_var = translate_team_var(entryFormat)

  /*
  val matchesP : Algebra[Algebra[DinMacsVar]]
    = generate_matches_for_day(entryFormat)
      .map(translate_team_encoding >>> (it => it.map(trans_team_var)))
  val matches = flatten(matchesP)
  */
  val restriction1 : Algebra[DinMacsVar]
    = generate_twice_in_a_row_restriction(entryFormat).map(trans_team_var)
  /*val restriction2 : Algebra[DinMacsVar]
    = generate_match_everybody_restriction(entryFormat).map(trans_team_var)
  */
  val formula : Algebra[DinMacsVar] =  to_CNF(restriction1) //to_CNF(restriction1 & matches & restriction2)
  val formula_cnf = formula//to_CNF(formula)
  val (total_clauses,clauses) = to_dinmacs_with_count(formula_cnf)
  val N = entryFormat.participants.length
  val M = entryFormat.start_date.until(entryFormat.end_date, ChronoUnit.DAYS).toInt
  val R = get_number_of_matches_per_day(entryFormat.start_time, entryFormat.end_time)
  val total_vars = N * (M+1) * R * 2
  "p cnf " + total_vars.toString + " " + total_clauses.toString + " \n" + clauses
}

}

