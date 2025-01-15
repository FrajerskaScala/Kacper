import scala.annotation.tailrec

def isStr[A](l: List[A], len: Int)(pred: A => Boolean): Boolean = {
  @tailrec
  def helper(lst: List[A], currentLen: Int, maxLen: Int): Boolean = lst match {
    case Nil => maxLen >= len
    case head :: tail =>
      if (pred(head)) {
        helper(tail, currentLen + 1, math.max(maxLen, currentLen + 1))
      } else {
        helper(tail, 0, maxLen)
      }
  }
  
  helper(l, 0, 0)
}

@main def zad1: Unit = {
  val l = List(1, 2, 6, 4, 1, 3, 5, 7, 10, 2, -1, 4, 5, 1, 3)
  val len = 3
  val pred = (a: Int) => a > 2
  
  println(isStr(l, len)(pred))
}