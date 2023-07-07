import net.fortuna.ical4j.model.Calendar
import net.fortuna.ical4j.model.component.VEvent
import net.fortuna.ical4j.model.property.{Attendee, Description, DtEnd, DtStamp, Duration, Uid}
import net.fortuna.ical4j.util.{RandomUidGenerator, UidGenerator}

import java.time.{LocalDate, LocalTime}
import java.util.UUID

case class TournamentGame(
                           date: LocalDate,
                           start_time: LocalTime,
                           local_player: String,
                           visiting_player: String
                         )

def export_calendar(tournament_name: String, games: Array[TournamentGame]): Calendar =
  var cal = new Calendar()
    .withProdId("iCal4J")
    .withDefaults()
    .getFluentTarget()

  val uidGen = new RandomUidGenerator();

  games.foreach(tg => {
    var gameEvent = new VEvent(
      tg.date.atTime(tg.start_time),
      f"$tournament_name Match: Local Player ${tg.local_player} VS Visiting Player ${tg.visiting_player}")

    gameEvent = gameEvent
      .withProperty(uidGen.generateUid())
      .withProperty(new Duration(java.time.Duration.ofHours(2)))
      .withProperty(new Description(f"Local Player: ${tg.local_player} Visiting Player: ${tg.visiting_player}"))
      .getFluentTarget()

    cal = cal.withComponent(gameEvent).getFluentTarget()
  })
  return cal
