package main.scala.collections

// #11
def encodeModified[A](list: List[A]): List[Any] = 
    encode(list).map( x => x match
        case (x, y) if x > 1 => (x, y)
        case (1, z) => z
    )

// #12
def decode[A](list: List[(Int, A)]): List[A] = list.flatMap( x => List.fill(x(0))(x(1)) )

// #13
def encodeDirect[A](list: List[A]): List[(Int, A)] =
    def packSublist(sublist: List[A]): (Int, A) = 
        @annotation.tailrec
        def helper(l: List[A], acc: List[A]): List[A] = l match
            case Nil => acc
            case head :: tail => tail.length match 
                    case 0 => acc :+ head
                    case _ => head == tail.head match
                        case true => helper(tail, acc :+ head)
                        case false => acc :+ head
        val res = helper(sublist, List[A]())
        (res.length , res.head)
    
    @annotation.tailrec
    def encodeHelper(list: List[A], acc: List[(Int, A)]): List[(Int, A)] = list match
        case Nil => acc
        case head :: tail => encodeHelper(list.drop(packSublist(list)(0)), acc :+ packSublist(list))

    encodeHelper(list, List[(Int, A)]())

// #14
def duplicate[A](list: List[A]) : List[A] = list.map( x => List(x, x) ).flatten

// #15
def duplicateN[A](n: Int, list: List[A]) : List[A] = list.map(x => List.fill(n)(x)).flatten

// #16
def drop[A](n: Int, list: List[A]): List[A] = 
    @annotation.tailrec
    def helper(l: List[A], i: Int, acc: List[A]): List[A] =
        l match
            case Nil => acc
            case head :: tail => i == n match
                case true => helper(tail, 1, acc)
                case false => helper(tail, i+1, acc :+ head)
    helper(list, 1, List[A]())

// #17
def split[A](n: Int, list: List[A]): (List[A], List[A]) = list.partition(p => list.indexOf(p) < n)

// #18
def slice[A](i: Int, k: Int, list: List[A]): List[A] = list.slice(i, k)

// #19
def rotate[A](m: Int, list: List[A]): List[A] = 
    val n = m.abs % list.length
    m match
        case i if i >= 0 => list.drop(n) ::: list.take(n)
        case _ => list.takeRight(n) ::: list.dropRight(n)

// #20
def removeAt[A](ind: Int, list: List[A]): (List[A], A) = ( list.patch(ind, Nil, 1), list(ind))