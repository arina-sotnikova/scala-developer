package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(x: Int, accum: Int): Int = {
      if( n <= 0) accum
      else loop(x - 1, x * accum)
    }
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */

  // without recursion
  def fibonacci(n: Int): Int = {
    if (n <= 1) n
    else {
      var curr = 1
      var prev = 0
      var counter = n
      while (counter != 2) {
        val tmp = curr
        curr = curr + prev
        prev = tmp
        counter -= 1
      }
      curr + prev
    }
  }

  // recursion
  def fibonacciRec(n: Int): Int = {
    if (n <= 1) n
    else fibonacciRec(n - 1) + fibonacciRec(n - 2)
  }

}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции


  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  def sum(x: Int, y: Int): Int = x + y


  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 5
}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal
  class Cat extends Animal

  //def treat(animal: Animal): Unit = ???
  //def treat(animal: Option[Animal]): Unit = ???

  //val d: Dog = ???
  //val dOpt: Option[Dog] = ???
  //treat(d)
  //treat(dOpt)

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Variance
  // 1. Invariance
  // 2. Covariance
  // 3. Contrvariance

  sealed trait Option[+T] {
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def get: T = {
      this match {
        case None => throw new NoSuchElementException("empty Option")
        case Some(v) => v
      }
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = {
      this match {
        case Some(v) => f(v)
        case None => None
      }
    }

    def printIfAny() = {
      this match {
        case Some(v) => println(v.toString)
        case None => ()
      }
    }

    def zip[R](v: Option[R]): Option[(T, R)]  = {
      (this, v) match {
        case (Some(t), Some(r)) => Some((t, r))
        case _ => None
      }
    }

    def filter(f: T => Boolean): Option[T] = {
      this match {
        case Some(t) if f(t) => this
        case _ => None
      }
    }
  }

  object Option{
    def apply[T](v: T): Option[T] = Some(v)
  }

  //val o1: Option[Int] = ???

  //val o2: Option[Int] = o1.map(_ + 2)

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  var o: Option[Animal] = None
  var i: Option[Int] = None


  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */


   sealed trait List[+T]{
     def ::[TT >: T](elem: TT): List[TT] = {
       new ::(elem, this)
     }

     def reverse: List[T] = List.reverse_(remainder = this)

     def map[R](f: T => R): List[R] = List.map_(this, f)

     def flatMap[R](f: T => List[R]): List[R] = List.flatten(this.map(f))

     def filter(f: T => Boolean): List[T] = List.filter_(this, f)

     def mkString(sep: String): String = List.mkString_(this, sep)
   }

   case class ::[T](head: T, tail: List[T]) extends List[T]
   case object Nil extends List[Nothing]

   object List{
     def apply[A](v: A*): List[A] = if(v.isEmpty) Nil
     else new ::(v.head, apply(v.tail:_*))

     private def reverse_[T](acc: List[T] = Nil, remainder: List[T]): List[T] = {
       remainder match {
         case Nil => acc
         case ::(head, tail) => reverse_(::(head, acc), tail)
       }
     }

     private def map_[T,R](list: List[T], f: T => R, acc: List[R] = Nil): List[R] = {
       list match {
         case Nil => acc.reverse
         case ::(head, tail) => map_(tail, f, ::(f(head), acc))
       }
     }

     private def concatLists[T](list1: List[T], list2: List[T], acc: List[T] = Nil): List[T] = {
       (list1, list2) match {
         case (Nil, Nil) => acc.reverse
         case (::(head, tail), snd) => concatLists(tail, snd, ::(head, acc))
         case (Nil, ::(head, tail)) => concatLists(Nil, tail, ::(head, acc))
       }
     }

     private def flatten[T](list: List[List[T]], acc: List[T] = Nil): List[T] = {
       list match {
         case Nil => acc.reverse
         case ::(head, tail) => flatten(tail, concatLists(head, acc))
       }
     }

     private def filter_[T](list: List[T], f: T => Boolean, acc: List[T] = Nil): List[T] = {
       list match {
         case Nil => acc.reverse
         case ::(head, tail) => filter_(
           list = tail, f = f, acc = if (f(head)) ::(head, acc) else acc
         )
       }
     }

     private def mkString_[T](list: List[T], sep: String, buf: String = ""): String = {
       list match {
         case Nil => buf
         case ::(head, tail) => mkString_(tail, sep, if (buf == "") head.toString else buf + sep + head.toString)
       }
     }

     def incList(list: List[Int]): List[Int] = {
      list.map(elem => elem + 1)
     }

     def shoutString(list: List[String]): List[String] = {
       list.map(elem => "!" + elem)
     }
   }

   val l1: List[Nothing] = List()
   val l2 = List(1, 2, 3)



    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */


    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

 }