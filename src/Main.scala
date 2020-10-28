import scala.annotation.tailrec


object Main extends App {



  var dniTyg = List[String](
    "Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela"
  )

  println("Zadanie 1a\n")

  var dniTygTxt = ""


  for(dzien <- dniTyg) {

    dniTygTxt += dzien

    if(dniTyg.last != dzien){
      dniTygTxt += ", "
    }

  }

  println(dniTygTxt)


  println("\n1b\n")

  dniTygTxt = ""

  for(d <- dniTyg) {

    if(d.toLowerCase().startsWith("")) {
      dniTygTxt += d + ", "
    }

  }
  dniTygTxt = dniTygTxt.dropRight(2)

  println(dniTygTxt)

  //1c
  println("\n1c\n")

  dniTygTxt = ""

  {
    var i = 0
    while(i < dniTyg.length){
      dniTygTxt += dniTyg(i) + ", "
      i += 1
    }
  }
  dniTygTxt = dniTygTxt.dropRight(2)

  println(dniTygTxt)


  //2a
  println("Zadanie 2a\n")

  dniTygTxt = ""

  def createStringWithCommas[T](list: List[T]): String = list match {
    case Nil => ""
    case _ => list.head + ", " + createStringWithCommas(list.tail)
  }


  dniTygTxt = createStringWithCommas(dniTyg).dropRight(2)
  println(dniTygTxt)

  //2b
  println("\nZadanie 2b\n")

  dniTygTxt = ""

  def createStringWithCommasDesc[T](list: List[T]): String = list match {

    case Nil => ""
    case _ => createStringWithCommasDesc(list.tail) + ", " + list.head

  }

  dniTygTxt = createStringWithCommasDesc(dniTyg).drop(2)
  println(dniTygTxt)

  //Zadanie3
  println("\nZadanie 3\n")

  dniTygTxt = ""

  def createStringWithCommasTail[T](list: List[T]): String = {

    @tailrec
    def appendStr[T](list: List[T], str: String): String = list match {
      case Nil => str.dropRight(2)
      case head :: tail => appendStr(tail, str + head + ", ")
    }
    appendStr(list, "")
  }

  println(createStringWithCommasTail(dniTyg))

  //Zadanie4
  println("Zadanie 4a\n")

  def createStringWithCommasFoldl(list: List[String]): String = {
    list.foldLeft(""){ (acc, item) =>
      if(acc.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFoldl(dniTyg))

  //4b
  println("\nZadanie 4b\n")

  def createStringWithCommasFoldr(list: List[String]): String = {
    list.foldRight(""){ (acc, item) =>
      if(item.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFoldr(dniTyg))

  //4c
  println("\nZadanie 4c\n")

  def createStringWithCommasFoldlOnlyP(list: List[String]): String = {
    list.filter(_.toLowerCase()
      .startsWith("p")
    ) .foldLeft(""){ (acc, item) =>
      if(acc.isEmpty) acc + item
      else acc + ", " + item
    }
  }

  println(createStringWithCommasFoldlOnlyP(dniTyg))


  //Zadanie 5
  println("\nZadanie 5\n")

  val produktCena = Map (
    "Mąka" -> 20d,
    "Makaron" -> 15d,
    "Olej" -> 25d,
    "Masło" -> 30d
  )
  println("Przed :")
  for(i <- produktCena) println(i._1 + " - " + i._2)
  println()

  val produktCena10proc = produktCena.transform((_, v) => v * 0.9)
  println("Przed zmianie:")
  for(i <- produktCena10proc) println(i._1 + " - " + i._2)


  //Zadanie 6
  println("6\n")

  val tuple1 = ("Mercedes", 2011, 2223.5)
  val tuple2 = ('M', Math.PI, true)

  def printTuple[A, B, C](tup: (A, B, C)): Unit = {
    println(tup._1 + " - " + tup._2 + " - " + tup._3)
  }

  printTuple(tuple1)
  printTuple(tuple2)

  //Zadanie 7
  println("\nZadanie 7\n")

  val texts = List[String] ("5", "dsa", "21", "das","5", "dsasa","1", "dsad")

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case _: Exception => None
    }
  }

  for(n <- texts) println(toInt(n))

  //Zadanie 8
  println("\nZadanie 8\n")

  def returnListWithNoZero(list: List[Int]): List[Int] = {

    @tailrec
    def accNewList(list: List[Int], listR: List[Int]): List[Int] = list match{
      case Nil => listR
      case head :: tail =>
        if(head == 0) accNewList(tail, listR)
        else accNewList(tail, listR.appended(head))
    }
    accNewList(list, List.empty[Int])
  }

  var withoutZeros = ""
  for(wrt <- returnListWithNoZero(List[Int] (6, 5, 3, 61, 7, 0, 1))){
    withoutZeros += wrt + ", "
  }
  println(withoutZeros)
  //println(re)
  //Zadanie 9
  println("\nZadanie 9\n")

  def increaseEveryElemByOne(list: List[Int]) = list.map(x => x + 1)

  var list = List[Int] (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println("Lista przed inkrementacją: " + list)
  list = increaseEveryElemByOne(list)
  println("Lista po inkrementacji: " + list)

  //Zadanie 10
  println("\nZadanie 10\n")

  val realNumbers = List[Double](Math.PI, -Math.E, -2.5, 10.2, -4.9, 12.0, 5.2, 14.2 )

  def returnListOfAbsValInRange(list: List[Double], r1: Int, r2: Int): List[Double] = {
    list.filter(x => x >= r1)
      .filter(x => x <= r2)
      .map(x => x.abs)
  }

  println("przed filtracją: " + realNumbers)
  println(" po filtracji: " + returnListOfAbsValInRange(realNumbers, -5, 12))

}
