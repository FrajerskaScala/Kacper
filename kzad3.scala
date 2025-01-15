import org.apache.pekko.actor.{Actor, ActorLogging, ActorSystem, Props}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random


case class Zlecenie(l: List[List[Double]])
case object Start
case class Sumuj(liczby: List[Double])
case class Blad(liczby: List[Double])
case class Wynik(n: Double)


class Koordynator extends Actor with ActorLogging {
  var pracownicy: List[ActorRef] = List.empty
  var wyniki: Map[ActorRef, Double] = Map.empty
  var oczekujacyPracownicy: Set[ActorRef] = Set.empty

  def receive: Receive = {
    case Zlecenie(l) =>
      log.info("Otrzymano zlecenie, tworzenie pracowników...")
      pracownicy = l.map(_ => context.actorOf(Props[Pracownik]())).toList
      context.become(przygotowany(l))
  }

  def przygotowany(l: List[List[Double]]): Receive = {
    case Start =>
      log.info("Rozpoczynanie przetwarzania...")
      oczekujacyPracownicy = pracownicy.toSet
      pracownicy.zip(l).foreach { case (pracownik, liczby) =>
        pracownik ! Sumuj(liczby)
      }

    case Wynik(suma) =>
      log.info(s"Otrzymano wynik: $suma od ${sender()}")
      wyniki += (sender() -> suma)
      oczekujacyPracownicy -= sender()
      if (oczekujacyPracownicy.isEmpty) {
        val maksymalnaSuma = wyniki.values.max
        log.info(s"Maksymalna suma wartości monet: $maksymalnaSuma")
        context.system.terminate()
      }

    case Blad(liczby) =>
      log.info(s"Otrzymano błąd od ${sender()}, poprawianie danych...")
      val poprawioneLiczby = liczby.filter(_ >= 0)
      sender() ! Sumuj(poprawioneLiczby)
  }
}


class Pracownik extends Actor with ActorLogging {
  def receive: Receive = {
    case Sumuj(liczby) =>
      if (liczby.exists(_ < 0)) {
        log.info(s"Znaleziono ujemne wartości w liście: $liczby")
        sender() ! Blad(liczby)
      } else {
        val suma = liczby.sum
        log.info(s"Obliczono sumę: $suma")
        sender() ! Wynik(suma)
      }
  }
}

@main
def zad3: Unit = {
  val system = ActorSystem("Kolokwium")
  implicit val timeout: Timeout = Timeout(5.seconds)

  
  val dane = List(List(1.0, 2.0, 3.0),List(-1.0, 5.0, 6.0),List(7.0, 8.0, 9.0))

  val koordynator = system.actorOf(Props[Koordynator](), "Koordynator")
  koordynator ! Zlecenie(dane)
  koordynator ! Start
}