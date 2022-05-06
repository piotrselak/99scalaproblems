package main.scala

@main
def main(): Unit = println( lsortFreq(List(List(3,2), List(5,6,7), List(9), List(), List(), List(), List(9, 8))) )

// #21
def insertAt[A](newElem: A, ind: Int, list: List[A]): List[A] =
    @annotation.tailrec
    def helper(l: List[A], elem: A, elemInd: Int, i: Int, acc: List[A]): List[A] =
        l match
            case Nil => acc
            case head :: tail => elemInd == i match
                case true => ((acc :+ elem) :+ head) ::: tail
                case false => helper(tail, elem, elemInd, i+1, acc :+ head)
    helper(list, newElem, ind, 0, List[A]())

// #22
def range(n: Int, k: Int) : List[Int] = List.range(n, k+1)

// #23
def randomSelect[A](n: Int, list: List[A]): List[A] =
    val rand = scala.util.Random
    @annotation.tailrec
    def helper(l: List[A], acc: List[A], i: Int): List[A] = i == n match
        case true => acc
        case false => 
            val randElem = rand.nextInt(l.length)
            helper(removeAt(randElem, l)(0), acc :+ l(randElem), i+1)
    helper(list, List[A](), 0)

// #24
def lotto(n: Int, m: Int) : List[Int] = List.fill(n)(scala.util.Random.nextInt(m+1))

// #25
def randomPermute[A](list: List[A]): List[A] = randomSelect(list.length, list)

// #26 TODO!
//def combinations[A](n: Int, list: List[A]) List[List[A]]

// #27 TODO!
//a
//def group3
//b
//def group

// #28
// a)
def lsort[A](list: List[List[A]]): List[List[A]] = list.sortBy(_.length)
// b)
def lsortFreq[A](list: List[List[A]]) : List[List[A]] = 
    def freqOfElemByLen(elem: List[A]) : Int = list.count(_.length == elem.length)
    list.sortBy(freqOfElemByLen)
