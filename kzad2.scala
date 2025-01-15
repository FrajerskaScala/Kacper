import scala.io.Source

@main def zad2: Unit = {
  
  val noworodki = Source
    .fromResource("noworodki.txt")
    .getLines
    .toList

  val dziewczynki = noworodki
    .map(_.split(";").toList) 
    .collect {
      case List("c", imie, data_ur, wzrost) => (data_ur, wzrost.toInt)
    }
    .groupBy { case (data_ur, _) => data_ur.split("\\.")(1).toInt }
    .map { case (miesiac, lista) => 
      (miesiac, lista.map(_._2).min)
    }
    .toList

 
  val zestawienie = dziewczynki
    .groupBy(_._2)
    .toList
    .sortBy(-_._1)
    .zipWithIndex 
    .flatMap { case ((wzrost, lista), index) =>
      lista.map { case (miesiac, _) => (index + 1, miesiac, wzrost) }
    }
    .sortBy(_._1)

  println(zestawienie)
}