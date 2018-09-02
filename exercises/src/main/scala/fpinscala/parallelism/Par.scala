package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence2[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    val seq: Par[List[List[A]]] = sequence(ps)
    map(seq)(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val i: Int = run(es)(n).get
      run(es)(choices(i))
    }

  def choiceWithChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val key = run(es)(key).get
      run(es)(choices(key))
    }

  def choiceWithChoiceMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceMap(cond)(Map(true -> t, false -> f))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = chooser(pa)(choices)

  def choiceWithChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def choiceMapWithChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(choices(_))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(v => v)

  def flatMapWithJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = join(map(pa)(choices(_)))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

  }

}

object Examples {

  import Par._

  def sum1(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum1(l) + sum1(r) // Recursively sum both halves and add the results together.
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1) {
      unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  def processInts(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] =
    if (ints.length <= 1) {
      unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(processInts(l)(f)), fork(processInts(r)(f)))(f)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] = processInts(ints)(_ max _)

  def words(ps: List[String]): Par[Int] = {
    def countSingle(s: String): Par[Int] = unit(s.split(" ").length)

    map(sequence(ps map countSingle))(_.sum)
  }

  def process[A, B](ps: List[A])(f: A => Par[B])(acc: List[B] => B): Par[B] = map(sequence(ps map f))(acc)


  def words2(ps: List[String]): Par[Int] = process(ps)(s => lazyUnit({
    println(Thread.currentThread().getName)
    s.split(" ").length
  }))(_.sum)

  def words3(ps: List[String]): Par[Int] = map(parMap(ps)(s => s.split(" ").length))(_.sum)

  def process2[A, B](ps: List[A])(f: A => B)(acc: List[B] => B): Par[B] = map(parMap(ps)(f))(acc)

  def words4(ps: List[String]): Par[Int] = process2(ps)(s => s.split(" ").length)(_.sum)

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(pa, pb)((a, b) => (c: C) => f(a, b, c)), pc)((cToD, c1) => cToD(c1))

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(map3(pa, pb, pc)((a, b, c) => (d: D) => f(a, b, c, d)), pd)(_ (_))
  }

  def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
  // map2(map4(pa, pb, pc, pd)((a, b, c, d) => (e: E) => f(a, b, c, d, e)), pe)(_ (_))
    map2(map2(map2(map2(pa, pb)((a, b) => (c: C) => (d: D) => (e: E) => f(a, b, c, d, e)), pc)(_ (_)), pd)(_ (_)), pe)(_ (_))

  def main(args: Array[String]): Unit = {
    val executorService = Executors.newFixedThreadPool(5)

    println(sum1(List(1, 2, 3).toIndexedSeq))
    println(sum(List(1, 2, 3).toIndexedSeq)(executorService).get(1, TimeUnit.SECONDS))

    println(max(List(1, 2, 3).toIndexedSeq)(executorService).get())

    val paragraphs = List("this is the first paragraph", "and this is the second paragraph", "here comes the third one", "and a fourth one")
    println(words(paragraphs)(executorService).get())
    println(words2(paragraphs)(executorService).get())
    println(words3(paragraphs)(executorService).get())
    println(words4(paragraphs)(executorService).get())

    executorService.shutdown()
  }
}

object ForkDeadLock {

  import Par._

  def main(args: Array[String]): Unit = {
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    println(equal(S)(a, fork(a)))
  }
}

/*
given:
  map(y)(id) == y
then:
  map(y)(f) == map(y)(f)
  map(map(y)(id))(f) == map(y)(f)
  map(map(y)(id))(f) == map(y)(f compose id)
  map(map(y)(g))(f) == map(y)(f compose g)
 */