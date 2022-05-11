package main.scala.collections


def last[A](list: List[A]): A = list.last

//#02
def penultimate[A](list: List[A]): A = list.dropRight(1).last

//#03
def nth[A](n: Int, list: List[A]): A = list(n)

//#04
def length[A](list: List[A]): Int = list.length

//#05
def reverse[A](list: List[A]): List[A] = list.reverse

//#06
def isPalindrome[A](list: List[A]): Boolean = list.reverse == list

//#07 
def flatten(list: List[Any]): List[Any] = list.flatMap {
    case innerList: List[Any] => flatten(innerList)
    case elem => List(elem)  
}

//#08
def compress[A](list: List[A]): List[A] = 
    @annotation.tailrec
    def helper(l: List[A], acc: List[A]): List[A] = l match
        case Nil => acc
        case head :: tail => head == acc.last match
            case true => helper(tail, acc)
            case false => helper(tail, acc :+ head)
    helper(list.tail, List(list.head))

//#09
def pack[A](list: List[A]): List[List[A]] = 
    def sublist(subList: List[A]): List[A] = 
        @annotation.tailrec
        def helper(l: List[A], acc: List[A]): List[A] = l match
            case Nil => acc
            case head :: tail => tail.length match 
                case 0 => acc :+ head
                case _ => head == tail.head match
                    case true => helper(tail, acc :+ head)
                    case false => acc :+ head
        helper(subList, List[A]())
    @annotation.tailrec
    def packSublists(l: List[A], acc: List[List[A]]): List[List[A]] = l match
        case Nil => acc
        case head :: tail => packSublists(l.drop(sublist(l).length), acc :+ sublist(l))
    packSublists(list, List[List[A]]())

//#10
def encode[A](list: List[A]): List[(Int, A)] = pack(list).map(x => (x.length, x.head))