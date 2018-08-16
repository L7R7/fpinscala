package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def main(args: Array[String]): Unit = {
    val rng = Simple(42)
    val (n1, rng2) = rng.nextInt
    println(n1)
    val (n2, rng3) = rng2.nextInt
    println(n2)

    println(ints(10)(rng3))

    val seq: Rand[List[Int]] = sequence(List(unit(1), unit(2), unit(3)))
    println(seq(Simple(11))._1)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n >= 0) (n, rng2)
    else if (n == Int.MinValue) (0, rng2)
    else (-n, rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n / (Int.MaxValue + 1), rng2)
  }

  def doubleWithMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int)(acc: List[Int], rng2: RNG): (List[Int], RNG) = {
      if (c <= 0) (acc, rng2)
      else {
        val (n, rng3) = rng2.nextInt
        go(c - 1)(n :: acc, rng3)
      }
    }

    go(count)(Nil, rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((a: Rand[A], acc: Rand[List[A]]) => map2(a, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = this.flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
  //    this.flatMap()
    for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = this.run(s)
    f(a).run(s2)
  })

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object Simulator {

  import State._

  def update: Input => Machine => Machine = (i: Input) =>
    (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
      case (Turn, Machine(true, _, _)) => s
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => update(i)).map(s => modify[Machine](s)))
    // _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    println(Simulator.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))._1)
  }
}
