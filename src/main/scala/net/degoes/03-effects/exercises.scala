// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.effects

import scalaz.zio._
import scalaz.zio.console._
import scala.concurrent.duration._

object io {
  sealed trait Program[+A] { self =>

    def map[A, B](f: A => B): Program[B] =
      self.flatMap((a: A) => Program.point(f(a)))

    def flatMap[A, B](f: A => Program[B]): Program[B] = ???
      // self match {
      //   case Program.Return(a) => f(a)
      //   case Program.WriteLine(line, next) =>
      //     Program.WriteLine(line, next.flatMap(f))
      //   case Program.ReadLine(next) =>
      //     Program.ReadLine((input: String) => next(input).flatMap(f))
      // }
  }
  object Program {
    def point[A](a: => A): Program[A] = new Return(() => a)

    def writeLine(line: String): Program[Unit] =
      WriteLine[Unit](line, point(()))

    def readLine: Program[String] =
      ReadLine[String](point(_))

    final case class Return[A](value: () => A) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
  }

  import Program.{point, writeLine, readLine}
  // We can do each of these by themselves, but we need #map and #flatMap in order to combine them and do anything
  // interesting.
  point(1)
  readLine
  writeLine("Foo")
}

object zio_background {
  sealed trait Program[A] { self =>
    final def *> [B](that: Program[B]): Program[B] = self.seq(that).map(_._2)

    final def <* [B](that: Program[B]): Program[A] = self.seq(that).map(_._1)

    final def map[B](f: A => B): Program[B] =
      flatMap(f andThen (Program.point(_)))

    final def seq[B](that: Program[B]): Program[(A, B)] =
      for {
        a <- self
        b <- that
      } yield (a, b)

    final def flatMap[B](f: A => Program[B]): Program[B] =
      self match {
        case Program.ReadLine(next) =>
          Program.ReadLine(input => next(input).flatMap(f))
        case Program.WriteLine(line, next) =>
          Program.WriteLine(line, next.flatMap(f))
        case Program.Return(value) =>
          f(value())
      }
  }
  object Program {
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class Return[A](value: () => A) extends Program[A]

    val readLine: Program[String] = ReadLine(point[String](_))
    def writeLine(line: String): Program[Unit] = WriteLine(line, point(()))
    def point[A](a: => A): Program[A] = Return(() => a)
  }

  import Program.{readLine, writeLine, point}

  val yourName1: Program[Unit] =
    writeLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
        writeLine("Hello, " + name + ", good to meet you!").map(_ =>
          ()
        )
      )
    )

  // Ignore me
  point(())

  //
  // EXERCISE 1
  //
  // Rewrite `yourName1` to use a for comprehension.
  //
  lazy val yourName2: Program[Unit] =
    for {
      _ <- writeLine("What is your name?")
      name <- readLine
      _ <- writeLine("Hello, " + name + ", good to meet you!")
    } yield ()

  //
  // EXERCISE 2
  //
  // Rewrite `yourName2` using the helper function `getName`, which shows how
  // to create larger programs from smaller programs.
  //
  lazy val yourName3: Program[Unit] =
  for {
    name <- getName
    _ <- writeLine("Hello, " + name + ", good to meet you!")
  } yield ()

  val getName: Program[String] =
    writeLine("What is your name?").flatMap(_ => readLine)

  //
  // EXERCISE 3
  //
  // Implement the following effectful procedure, which interprets
  // `Program[A]` into `A`. You can use this procedure to "run" programs.
  //
  def interpret[A](program: Program[A]): A =
    program match {
      case Program.Return(a) => a()
      case Program.WriteLine(line, next) =>
        println(line)
        interpret(next)
      case Program.ReadLine(next) =>
        interpret(next(scala.io.StdIn.readLine))
    }

  //
  // EXERCISE 4
  //
  // Implement the following function, which shows how to write a combinator
  // that operates on programs.
  //
  def sequence[A](programs: List[Program[A]]): Program[List[A]] = ???
    // p.seq(sequence(ps)).map { case (a, as) => a :: as }
    // programs match {
    //   case Nil => Program.point(Nil)
    //   case p :: ps =>
    //     val a : Program[A] = p
    //     val as: Program[List[A]] = sequence(ps)
    //     a.zip(as).map {
    //       case (a, as) => a :: as
    //     }
    // }

  //
  // EXERCISE 5
  //
  // Implement the following function, which can be thought of as an effectful
  // "for loop" that collects the result of each iteration.
  //
  def forEach[A, B](values: List[A])(body: A => Program[B]): Program[List[B]] =
    sequence(values.map(body))
  def forEach[A, B](as: A*)(body: A => Program[B]): Program[List[B]] =
    forEach(as.toList)(body)
  val example: Program[List[Unit]] =
    forEach("Hello", "World", "Each", "On", "Its", "Own", "Line")(text =>
      writeLine(text)
    )

  //
  // EXERCISE 6
  //
  // Translate the following procedural program into a purely functional program
  // using `Program` and a for comprehension.
  //
  def ageExplainer1(): Unit = {
    println("What is your age?")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case Some(age) =>
        if (age < 12) println("You are a kid")
        else if (age < 20) println("You are a teenager")
        else if (age < 30) println("You are a grownup")
        else if (age < 50) println("You are an adult")
        else if (age < 80) println("You are a mature adult")
        else if (age < 100) println("You are elderly")
        else println("You are probably lying.")
      case None =>
        println("That's not an age, try again")

        ageExplainer1()
    }
  }

  def ageExplainer2: Program[Unit] =
    for {
      _ <- writeLine("What is your age?")
      ageStr <- readLine
      _ <- scala.util.Try(ageStr.toInt).toOption match {
        case Some(age: Int) =>
          writeLine(
            if (age < 12) "You are a kid"
            else if (age < 20) "You are a teenager"
            else if (age < 30) "You are a grownup"
            else if (age < 50) "You are an adult"
            else if (age < 80) "You are a mature adult"
            else if (age < 100) "You are elderly"
            else "You are probably lying."
          )
        case None =>
          writeLine("That's not an age, try again") *> ageExplainer2
      }
    } yield ()
}

object zio_type {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Write the type of `IO` values that can fail with an `Exception`, or
  // may return an `A`.
  //
  type Exceptional[A] = IO[Exception, A]

  //
  // EXERCISE 2
  //
  // Write the type of `IO` values that can fail with a `Throwable`, or
  // may return an `A`.
  //
  type Task[A] = IO[Throwable, A]

  //
  // EXERCISE 3
  //
  // Write the type of `IO` values that cannot fail, but may return an `A.`
  //
  type NonFailing[A] = IO[Nothing, A]

  //
  // EXERCISE 4
  //
  // Write the type of `IO` values that cannot return a value, but may fail
  // with an `E`.
  //
  type NonReturning[E] = IO[E, Nothing]

  //
  // EXERCISE 5
  //
  // Write the type of `IO` values that cannot fail or return a value.
  //
  type NonTerminating = IO[Nothing, Nothing]
}

object zio_values {
  //
  // EXERCISE 1
  //
  // Using the `IO.now` method, lift the integer `2` into a strictly-evaluated
  // `IO`.
  //
  val ioInteger: IO[Nothing, Int] = IO.now(2)

  //
  // EXERCISE 2
  //
  // Using the `IO.point` method, lift the string "Functional Scala" into a
  // lazily-evaluated `IO`.
  //
  val ioString: IO[Nothing, String] = IO.point("Functional Scala")

  //
  // EXERCISE 3
  //
  // Using the `IO.fail` method to lift the string "Bad Input" into a failed
  // `IO`.
  //
  val failedInput: IO[String, Nothing] =
  IO.fail("Bad Input")
}

object zio_composition {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Map the `IO[Nothing, Int]` into an `IO[Nothing, String]` by converting the
  // integer into its string rendering using the `map` method of the `IO`
  // object.
  //
  (IO.point(42).map(_.toString) : IO[Nothing, String])

  //
  // EXERCISE 2
  //
  // Map the `IO[Int, Nothing]` into an `IO[String, Nothing]` by converting the
  // integer error into its string rendering using the `leftMap` method of the
  // `IO` object.
  //
  (IO.fail(42).leftMap(_.toString) : IO[String, Nothing])

  //
  // EXERCISE 3
  //
  // Using the `flatMap` and `map` methods of `IO`, add `ioX` and `ioY`
  // together.
  //
  val ioX: IO[Nothing, Int] = IO.point(42)
  val ioY: IO[Nothing, Int] = IO.point(58)
  val ioXPlusY: IO[Nothing, Int] = ioX.flatMap(x => ioY.map(y => x + y))

  //
  // EXERCISE 4
  //
  // Using the `flatMap` method of `IO`, implement `ifThenElse`.
  //
  def ifThenElse[E, A](bool: IO[E, Boolean])(
    ifTrue: IO[E, A], ifFalse: IO[E, A]): IO[E, A] =
    bool.flatMap(_ match { case true => ifTrue; case false => ifFalse })
  val exampleIf = ifThenElse(IO.point(true))(IO.point("It's true!"), IO.point("It's false!"))

  //
  // EXERCISE 5
  //
  // Translate the following program, which uses for-comprehensions, to its
  // equivalent chain of `flatMap`'s, followed by a final `map`.
  //
  for {
    v1 <- IO.point(42)
    v2 <- IO.point(58)
  } yield "The total is: " + (v1 + v2).toString

  //
  // EXERCISE 6
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def decode1(read: () => Byte): Either[Byte, Int] = {
    val b = read()
    if (b < 0) Left(b)
    else {
      Right(b.toInt +
      (read().toInt << 8) +
      (read().toInt << 16) +
      (read().toInt << 24))
    }
  }

  def decode2[E](read: IO[E, Byte]): IO[E, Either[Byte, Int]] = {
    def pack(shift: Int): IO[E, Int] =
      read.map(_.toInt << (8 * shift))

    for {
      b <- read
      r <-
        if (b < 0) IO.now(Left(b))
        else {

          for {
            i1 <- pack(1)
            i2 <- pack(2)
            i3 <- pack(3)
          } yield Right(
            b.toInt + i1 + i2 + i3
          )
        }
    } yield r
  }

  //
  // EXERCISE 7
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def getName1(print: String => Unit, read: () => String): Option[String] = {
    print("Do you want to enter your name?")
    read().toLowerCase.take(1) match {
      case "y" => Some(read())
      case _ => None
    }
  }
  def getName2[E](print: String => IO[E, Unit], read: IO[E, String]): IO[E, Option[String]] =
    for {
      _ <- print("Do you want to enter your name?")
      v <- read
      r <- if (v.toLowerCase.take(1) == "y") read.map(Some.apply)
      else IO.now(None)
    } yield r


  //
  // EXERCISE 8
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def forever1(action: () => Unit): Unit =
    while (true) action()
  def forever2[A](action: IO[Nothing, A]): IO[Nothing, Nothing] =
    action *> forever2(action)

  //
  // EXERCISE 9
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def repeatN1(n: Int, action: () => Unit): Unit =
    if (n <= 0) ()
    else {
      action()
      repeatN1(n - 1, action)
    }
  def repeatN2[E](n: Int, action: IO[E, Unit]): IO[E, Unit] =
    if (n <= 0) IO.unit
    else action *> repeatN2(n - 1, action)

  //
  // EXERCISE 10
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) *> IO.point(19)
  IO.point(42).flatMap(_ => IO.point(19))
  IO.point(42).map(_ => 19)

  //
  // EXERCISE 11
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) <* IO.point(19)
  IO.point(42).flatMap(n => IO.point(19).map(_ => n))

  //
  // EXERCISE 12
  //
  // Translate the following expression into an equivalent expression using
  // the `map` and `flatMap` methods of the `IO` object.
  //
  (IO.point(42) <* IO.point(19)) *> IO.point(1)
}

object zio_failure {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.fail` method, create an `IO[String, Int]` value that
  // represents a failure with a string error message, containing a user-
  // readable description of the failure.
  //
  val stringFailure1: IO[String, Int] =
    ???

  //
  // EXERCISE 2
  //
  // Using the `IO.fail` method, create an `IO[Int, String]` value that
  // represents a failure with an integer error code.
  //
  val intFailure: IO[Int, String] = ???

  //
  // EXERCISE 3
  //
  // Transform the error of `intFailure` into its string representation using
  // the `leftMap` method of `IO`.
  //
  val stringFailure2: IO[String, String] = ???

  //
  // EXERCISE 4
  //
  // Translate the following exception-throwing program into its ZIO equivalent.
  //
  def accessArr1[A](i: Int, a: Array[A]): A =
    if (i < 0 || i >= a.length) throw new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")")
    else a(i)
  def accessArr2[A](i: Int, a: Array[A]): IO[IndexOutOfBoundsException, A] =
    if (i < 0 || i >= a.length) IO.fail(new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")"))
    else IO.point(a(i))

  //
  // EXERCISE 5
  //
  // Translate the following ZIO program into its exception-throwing equivalent.
  //
  def divide1(n: Int, d: Int): IO[ArithmeticException, Int] =
    if (d == 0) IO.fail(new ArithmeticException)
    else IO.now(n / d)
  def divide2(n: Int, d: Int): Int =
    if (d == 0) throw new ArithmeticException
    else n / d

  //
  // EXERCISE 6
  //
  // Recover from a division by zero error by returning `-1`.
  //
  val recovered1: IO[Nothing, Int] =
    divide1(100, 0).attempt.map {
      case Left(error) => ???
      case Right(value) => ???
    }

  //
  // EXERCISE 7
  //
  // Recover from a division by zero error by using `redeem`.
  //
  val recovered2: IO[Nothing, Int] =
    divide1(100, 0).redeem(
      _ => IO.now(-1),
      x => IO.now(x)
    )

  //
  // EXERCISE 8
  //
  // Use the `orElse` method of `IO` to try `firstChoice`, and fallback to
  // `secondChoice` only if `firstChoice` fails.
  //
  val firstChoice: IO[ArithmeticException, Int] = divide1(100, 0)
  val secondChoice: IO[Nothing, Int] = IO.now(400)
  val combined: IO[Nothing, Int] = firstChoice orElse secondChoice
}

object zio_effects {
  import scala.io.StdIn.readLine
  import scala.io.Source
  import java.io.File
  import java.util.concurrent.{Executors, TimeUnit}

  type ??? = Nothing
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.sync` method, wrap Scala's `println` method to import it into
  // the world of pure functional programming.
  //
  def putStrLn(line: String): IO[Nothing, Unit] = IO.sync(println(line))

  //
  // EXERCISE 2
  //
  // Using the `IO.sync` method, wrap Scala's `readLine` method to import it
  // into the world of pure functional programming.
  //
  val getStrLn: IO[Nothing, String] = IO.sync(readLine)

  //
  // EXERCISE 3
  //
  // Using the `IO.syncException` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile1(file: File): IO[Exception, List[String]] =
    IO.syncException(
      Source.fromFile(file).getLines.toList
    )

  //
  // EXERCISE 4
  //
  // Using the `IO.syncThrowable` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile2(file: File): IO[Throwable, List[String]] =
    IO.syncThrowable(
      Source.fromFile(file).getLines.toList
    )

  //
  // EXERCISE 5
  //
  // Using the `IO.syncCatch` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  import java.io.IOException
  def readFile3(file: File): IO[IOException, List[String]] =
    IO.syncCatch(
      Source.fromFile(file).getLines.toList
    ) {
      case e: IOException => e
    }

  //
  // EXERCISE 6
  //
  // Identify the correct method and error type to import `System.nanoTime`
  // safely into the world of pure functional programming.
  //
  val nanoTime: IO[Nothing, Long] = IO.sync { System.nanoTime() }

  //
  // EXERCISE 7
  //
  // Identify the correct method, error, and value type to import `System.exit`
  // safely into the world of pure functional programming.
  //
  def sysExit(code: Int): IO[SecurityException, Nothing] =
    IO.syncCatch(System.exit(code)) {
      case e: SecurityException => e
    } *> IO.never

  //
  // EXERCISE 8
  //
  // Identify the correct method, error, and value type to import
  // `Array.update` safely into the world of pure functional programming.
  //
  def arrayUpdate[A](a: Array[A], i: Int, f: A => A): IO[ArrayIndexOutOfBoundsException, Unit] =
    IO.syncCatch(
      a.update(i, f(a(i)))
    ) {
      case e: ArrayIndexOutOfBoundsException => e
    }

  //
  // EXERCISE 9
  //
  // Use the `IO.async` method to implement the following `sleep` method, and
  // choose the correct error type.
  //
  val scheduledExecutor = Executors.newScheduledThreadPool(1)
  def sleep(l: Long, u: TimeUnit): IO[Nothing, Unit] =
  IO.async[Nothing, Unit](callback =>
    scheduledExecutor.schedule(new Runnable {
      def run(): Unit = callback(ExitResult.Completed())
    }, l, u)
  )

  //
  // EXERCISE 10
  //
  // Wrap the following Java-esque callback API into an `IO` using `IO.async`.
  //
  def readChunk(success: Array[Byte] => Unit, failure: Throwable => Unit): Unit = ???
  val readChunkIO: IO[Throwable, Array[Byte]] =
    IO.async[Throwable, Array[Byte]](callback =>
      readChunk(
        bytes => callback(ExitResult.Completed(bytes)),
        error => callback(ExitResult.Failed(error))
      )
    )

  //
  // EXERCISE 11
  //
  // Translate the following procedural program into ZIO.
  //
  def playGame1(): Unit = {
    val number = scala.util.Random.nextInt(5)
    println("Enter a number between 0 - 5: ")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case None =>
        println("You didn't enter an integer!")
        playGame1
      case Some(guess) if (guess == number) =>
        println("You guessed right! The number was " + number)
      case _ =>
        println("You guessed wrong! The number was " + number)
    }
  }

  val playGame2: IO[Exception, Unit] =
    for {
      number <- IO.sync(
        scala.util.Random.nextInt(5)
      )
      _ <- putStrLn("Enter a number between 0 - 5: ")
      _ <- getStrLn.flatMap(str =>
        scala.util.Try(str.toInt).toOption match {
          case None =>
            putStrLn("You didn't enter an integer!") *> playGame2
          case Some(guess) if (guess == number) =>
            putStrLn("You guessed right! The number was " + number)
          case _ =>
            putStrLn("You guessed wrong! The number was " + number)
        })
    } yield ()

}

object zio_concurrency {
  type ??? = Nothing

  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Race `leftContestent1` and `rightContestent1` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent1 = IO.never
  val rightContestent1 = putStrLn("Hello World")
  val raced1: IO[java.io.IOException, Unit] =
    leftContestent1 race rightContestent1

  //
  // EXERCISE 2
  //
  // Race `leftContestent2` and `rightContestent2` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent2: IO[Exception, Nothing] = IO.fail(new Exception("Uh oh!"))
  val rightContestent2: IO[Exception, Unit] = IO.sleep(10.milliseconds) *> putStrLn("Hello World")
  val raced2: IO[Exception, (Nothing, Unit)] = leftContestent2 par rightContestent2

  //
  // EXERCISE 3
  //
  // Compute `leftWork1` and `rightWork1` in parallel using the `par` method of
  // `IO`.
  //
  val leftWork1: IO[Nothing, Int] = fibonacci(10)
  val rightWork1: IO[Nothing, Int] = fibonacci(10)
  val par1: IO[Nothing, (Int, Int)] = leftWork1 par rightWork1


  //
  // EXERCISE 4
  //
  // Compute all values `workers` in parallel using `IO.parAll`.
  //
  val workers: List[IO[Nothing, Int]] = (1 to 10).toList.map(fibonacci(_))
  val workersInParallel: IO[Nothing, List[Int]] =
    IO.parAll(workers)

  //
  // EXERCISE 5
  //
  // Implement `myPar` by forking `left` and `right`, and then joining them
  // and yielding a tuple of their results.
  //
  def myPar[E, A, B](left: IO[E, A], right: IO[E, B]): IO[E, (A, B)] =
    ???

  //
  // EXERCISE 6
  //
  // Use the `IO.supervise` method to ensure that when the main fiber exits,
  // all fibers forked within it will be terminated cleanly.
  //
  val supervisedExample: IO[Nothing, Unit] =
    IO.supervise(for {
      fiber <- fibonacci(10000).fork
    } yield ())

  //
  // EXERCISE 7
  //
  // Use the `interrupt` method of the `Fiber` object to cancel the long-running
  // `fiber`.
  //
  val interrupted1: IO[Nothing, Unit] =
    for {
      fiber <- fibonacci(10000).fork
    } yield ()

  //
  // EXERCISE 8
  //
  // Use the `zipWith` method of the `Fiber` object to combine `fiber1` and
  // `fiber2` into a single fiber (by summing the results), so they can be
  // interrupted together.
  //
  val interrupted2: IO[Nothing, Unit] =
    for {
      fiber1 <- fibonacci(10).fork
      fiber2 <- fibonacci(20).fork
      both = (fiber1 zip fiber2)
      _      <- both.interrupt
    } yield ()

  //
  // EXERCISE 9
  //
  // Use the `timeout` method of `IO` to time out the following long-lived
  // computation after 60 seconds.
  //
  val timedout: IO[Nothing, Option[Int]] =
    fibonacci(100) ?

  //
  // EXERCISE 10
  //
  // Use `IO.parTraverse` to compute the fibonacci numbers of the list of
  // integers in parallel.
  //
  val fibsToCompute = List(1, 2, 3, 4, 5, 6, 7)
  val inParallel: IO[Nothing, List[Int]] =
    IO.parTraverse(fibsToCompute)(n => fibonacci(n))

  def fibonacci(n: Int): IO[Nothing, Int] =
    if (n <= 1) IO.now(n)
    else fibonacci(n - 1).seqWith(fibonacci(n - 2))(_ + _)

  def fibonacciPar(n: Int): IO[Nothing, Int] =
    if (n <= 1) IO.now(n)
    else fibonacci(n - 1).parWith(fibonacci(n - 2))(_ + _)
}

object zio_resources {
  import java.io.{File, FileInputStream}
  class InputStream private (is: FileInputStream) {
    def read: IO[Exception, Option[Byte]] =
      IO.syncException(is.read).map(i => if (i < 0) None else Some(i.toByte))
    def close: IO[Exception, Unit] =
      IO.syncException(is.close())
  }
  object InputStream {
    def openFile(file: File): IO[Exception, InputStream] =
      IO.syncException(new InputStream(new FileInputStream(file)))
  }

  object classic {
    trait Handle
    def openFile(file: String): Handle = ???
    def closeFile(handle: Handle): Unit = ???
    def readFile(handle: Handle): Array[Byte] = ???

    // Classic paradigm for safe resource handling using
    // try / finally:
    def safeResource(file: String): Unit = {
      var handle: Handle = null.asInstanceOf[Handle]

      try {
        handle = openFile(file)

        readFile(handle)
      } finally if (handle != null) closeFile(handle)
    }

    def finallyPuzzler(): Unit = {
      try {
        try throw new Error("e1")
        finally throw new Error("e2")
      } catch {
        case e : Error => println(e)
      }
    }
  }

  //
  // EXERCISE 1
  //
  // Rewrite the following procedural program to ZIO, using `IO.fail` and the
  // `ensuring` method of the `IO` object.
  //
  def tryCatch1(): Unit =
    try throw new Exception("Uh oh")
    finally println("On the way out...")
  val tryCatch2: IO[Exception, Unit] =
    IO.fail(new Exception("Uh oh"))
    .ensuring(
      putStrLn("On the way out").attempt.void
    )

  //
  // EXERCISE 2
  //
  // Rewrite the `readFile1` function to use `bracket` so resources can be
  // safely cleaned up in the event of errors, defects, or interruption.
  //
  def readFile1(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    for {
      stream <- InputStream.openFile(file)
      bytes  <- readAll(stream, Nil)
      _      <- stream.close
    } yield bytes
  }

  def readFile2(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    InputStream.openFile(file)
      .bracket(_.close.attempt.void) { is =>
        readAll(is, Nil)
      }
  }

  //
  // EXERCISE 3
  //
  // Implement the `tryCatchFinally` method using `bracket` or `ensuring`.
  //
  def tryCatchFinally[E, A]
    (try0: IO[E, A])
    (catch0: PartialFunction[E, IO[E, A]])
    (finally0: IO[Nothing, Unit]): IO[E, A] =
    ???

  //
  // EXERCISE 4
  //
  // Use the `bracket` method to rewrite the following snippet to ZIO.
  //
  def readFileTCF1(file: File): List[Byte] = {
    var fis : FileInputStream = null

    try {
      fis = new FileInputStream(file)
      val array = Array.ofDim[Byte](file.length.toInt)
      fis.read(array)
      array.toList
    } catch {
      case e : java.io.IOException => Nil
    } finally if (fis != null) fis.close()
  }
  def readFileTCF2(file: File): IO[Exception, List[Byte]] =
    ???
}

object zio_ref {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  // The point of Ref is that it's mutable. It can be used as shared state among different fibers

  //
  // EXERCISE 1
  //
  // Using the `Ref.apply` constructor, create a `Ref` that is initially `0`.
  //
  val makeZero: IO[Nothing, Ref[Int]] =
    Ref(0)

  //
  // EXERCISE 2
  //
  // Using the `get` and `set` methods of the `Ref` you created, change the
  // value to be 10 greater than its initial value. Return the new value.
  //
  val incrementedBy10: IO[Nothing, Int] =
    for {
      ref   <- makeZero
      value <- ref.get
      _     <- ref.set(value + 10)
      value <- ref.get
    } yield value

  //
  // EXERCISE 3
  //
  // Using the `update` method of `Ref`, atomically increment the value by 10.
  // Return the new value.
  //
  val atomicallyIncrementedBy10: IO[Nothing, Int] =
    for {
      ref   <- makeZero
      value <- ref.update(x => x + 10)
    } yield value

  //
  // EXERCISE 4
  //
  // Using the `modify` method of `Ref` to atomically increment the value by 10,
  // but return the old value.
  //
  val atomicallyIncrementedBy10PlusGet: IO[Nothing, Int] =
    for {
      ref   <- makeZero
      value <- ref.modify(v => (v, v + 10))
    } yield value
}

object zio_promise {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `make` method of `Promise`, construct a promise that cannot
  // fail but can be completed with an integer.
  //
  val makeIntPromise: IO[Nothing, Promise[Nothing, Int]] =
  Promise.make

  //
  // EXERCISE 2
  //
  // Using the `complete` method of `Promise`, complete a promise constructed
  // with `makeIntPromise` with the integer 42.
  //
  val completed1: IO[Nothing, Boolean] =
    for {
      promise   <- makeIntPromise
      completed <- (promise ? : IO[Nothing, Boolean])
    } yield completed

  //
  // EXERCISE 3
  //
  // Using the `error` method of `Promise`, try to complete a promise
  // constructed with `makeIntPromise`. Explain your findings.
  //
  val errored1: IO[Nothing, Boolean] =
    for {
      promise   <- makeIntPromise
      completed <- (promise ? : IO[Nothing, Boolean])
    } yield completed

  //
  // EXERCISE 4
  //
  // Using the `error` method of `Promise`, complete a new promise that
  // you construct with `Promise.make` which can fail for any `Error` or
  // produce a `String`.
  //
  val errored2: IO[Nothing, Boolean] =
    for {
      promise   <- Promise.make[Error, String]
      completed <- (promise ? : IO[Nothing, Boolean])
    } yield completed

  //
  // EXERCISE 5
  //
  // Using the `interrupt` method of `Promise`, complete a new promise that
  // you construct with `Promise.make` which can fail for any `Error` or
  // produce a `String`.
  //
  val interrupted: IO[Nothing, Boolean] =
    for {
      promise   <- Promise.make[Error, String]
      completed <- (promise ? : IO[Nothing, Boolean])
    } yield completed

  //
  // EXERCISE 6
  //
  // Using the `get` method of `Promise`, retrieve a value computed from inside
  // another fiber.
  //
  val handoff1: IO[Nothing, Int] =
    for {
      promise <- Promise.make[Nothing, Int]
      _       <- (IO.sleep(10.seconds) *> promise.complete(42)).fork
      _       <- putStrLn("Waiting for promise to be completed...").attempt.void
      value   <- (promise ? : IO[Nothing, Int])
      _       <- putStrLn("Got: " + value).attempt.void
    } yield value

  //
  // EXERCISE 7
  //
  // Using the `get` method of `Promise`, try to retrieve a value from a promise
  // that was failed in another fiber.
  //
  val handoff2: IO[Error, Int] =
    for {
      promise <- Promise.make[Error, Int]
      _       <- (IO.sleep(10.seconds) *> promise.error(new Error("Uh oh!"))).fork
      _       <- putStrLn("Waiting for promise to be completed...").attempt.void
      value   <- (promise ? : IO[Error, Int])
      _       <- putStrLn("This line will NEVER be executed").attempt.void
    } yield value

  //
  // EXERCISE 8
  //
  // Using the `get` method of `Promise`, try to retrieve a value from a promise
  // that was interrupted in another fiber.
  //
  val handoff3: IO[Error, Int] =
    for {
      promise <- Promise.make[Error, Int]
      _       <- promise.interrupt.delay(10.milliseconds).fork
      value   <- (promise ? : IO[Error, Int])
    } yield value
}

object zio_queue {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `Queue.bounded`, create a queue for `Int` values with a capacity
  // of 10.
  //
  val makeQueue: IO[Nothing, Queue[Int]] =
  Queue.bounded(10)

  //
  // EXERCISE 2
  //
  // Using the `offer` method of `Queue`, place an integer value into a queue.
  //
  val offered1: IO[Nothing, Unit] =
    for {
      queue <- makeQueue
      _     <- (queue.offer(1) : IO[Nothing, Unit])
    } yield ()

  //
  // EXERCISE 3
  //
  // Using the `take` method of `Queue`, take an integer value from a queue.
  //
  val taken1: IO[Nothing, Int] =
    for {
      queue <- makeQueue
      _     <- queue.offer(42)
      value <- (queue.take: IO[Nothing, Int])
    } yield value

  //
  // EXERCISE 4
  //
  // In a child fiber, place 2 values into a queue, and in the main fiber, read
  // 2 values from the queue.
  //
  val offeredTaken1: IO[Nothing, (Int, Int)] =
    for {
      queue <- makeQueue
      _     <- (??? : IO[Nothing, Unit]).fork
      v1    <- (queue ? : IO[Nothing, Int])
      v2    <- (queue ? : IO[Nothing, Int])
    } yield (v1, v2)

  //
  // EXERCISE 5
  //
  // In a child fiber, read infintely many values out of the queue and write
  // them to the console. In the main fiber, write 100 values into the queue,
  // using `IO.sequence` on a `List` containing `queue.offer` programs.
  //
  val infiniteReader1: IO[Nothing, List[Unit]] =
    for {
      queue <- makeQueue
      _     <- (queue.take.flatMap(int => putStrLn(int.toString).forever): IO[Exception, Nothing]).fork
      vs    <- (IO.traverse(1 to 100)(queue.offer): IO[Nothing, List[Unit]])
    } yield vs

  //
  // EXERCISE 6
  //
  // Using `Queue`, `Ref`, and `Promise`, implement an "actor" like construct
  // that can atomically update the values of a counter.
  //
  sealed trait Message
  case class Increment(amount: Int) extends Message
  val makeCounter: IO[Nothing, Message => IO[Nothing, Int]] =
    for {
      counter  <- Ref(0)
      mailbox  <- Queue.bounded[(Message, Promise[Nothing, Int])](100)
      _        <- (mailbox.take ? : IO[Nothing, Fiber[Nothing, Nothing]])
    } yield { (message: Message) =>
      ???
    }

  val counterExample: IO[Nothing, Int] =
    for {
      counter <- makeCounter
      _       <- IO.parAll(List.fill(100)(IO.traverse((0 to 100).map(Increment(_)))(counter)))
      value   <- counter(Increment(0))
    } yield value
}

object zio_schedule {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using `Schedule.recurs`, create a schedule that recurs 5 times.
  //
  val fiveTimes: Schedule[Any, Int] =
    ???

  //
  // EXERCISE 2
  //
  // Using the `repeat` method of the `IO` object, repeat printing "Hello World"
  // five times to the console.
  //
  val repeated1 = putStrLn("Hello World") ?

  //
  // EXERCISE 3
  //
  // Using `Schedule.spaced`, create a schedule that recurs forever every 1
  // second.
  //
  val everySecond: Schedule[Any, Int] =
    ???

  //
  // EXERCISE 4
  //
  // Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats fives times,
  // every second.
  //
  val fiveTimesEverySecond =
    ???

  //
  // EXERCISE 5
  //
  // Using the `repeat` method of the `IO` object, repeat the action
  // putStrLn("Hi hi") using `fiveTimesEverySecond`.
  //
  val repeated2 = putStrLn("Hi hi") ?

  //
  // EXERCISE 6
  //
  // Using the `andThen` method of the `Schedule` object, the `fiveTimes`
  // schedule, and the `everySecond` schedule, create a schedule that repeats
  // fives times rapidly, and then repeats every second forever.
  //
  val fiveTimesThenEverySecond =
    ???

  //
  // EXERCISE 7
  //
  // Using the `retry` method of the `IO` object, retry the following error
  // a total of five times.
  //
  val error1 = IO.fail("Uh oh!")
  val retried5 = error1 ?

  //
  // EXERCISE 8
  //
  // Using the `||` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats the minimum
  // of five times and every second.
  //
  val fiveTimesOrEverySecond =
    ???

  //
  // EXERCISE 9
  //
  // Using `Schedule.exponential`, create an exponential schedule that starts from
  // 10 milliseconds.
  //
  val exponentialSchedule: Schedule[Any, Int] = ???

  //
  // EXERCISE 10
  //
  // Using the `jittered` method on `Schedule` objects, produced a jittered version of
  // `exponentialSchedule`.
  //
  val jitteredExponential = exponentialSchedule ?

  //
  // EXERCISE 11
  //
  // Using the `whileOutput` method on `Schedule`, produce a filtered schedule from
  // `Schedule.forever` that will halt when the number of recurrences exceeds 100.
  //
  val oneHundred = Schedule.forever.whileOutput(???)

  //
  // EXERCISE 12
  //
  // Using `Schedule.identity`, produce a schedule that recurs forever,
  // returning its inputs.
  //
  def inputs[A]: Schedule[A, A] = ???

  //
  // EXERCISE 13
  //
  // Using the `collect` method of `Schedule`, produce a schedule that recurs
  // forever, collecting its inputs into a list.
  //
  def collectedInputs[A]: Schedule[A, List[A]] = inputs[A] ?

  //
  // EXERCISE 14
  //
  // Using `*>`, combine `fiveTimes` and `everySecond` but return the output
  // of `everySecond`.
  //
  val fiveTimesEverySecondR: Schedule[Any, Int] = fiveTimes *> everySecond

  //
  // EXERCISE 15
  //
  // Produce a jittered schedule that first does exponential spacing (starting
  // from 10 milliseconds), but then after the spacing reaches 60 seconds,
  // switches over to fixed spacing of 60 seconds between recurrences, but will
  // only do that for up to 100 times, and produce a list of the results.
  //
  def mySchedule[A]: Schedule[A, List[A]] =
    ???
}

object zio_interop {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  import scala.concurrent.Future
  import scalaz.zio.interop.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  //
  // EXERCISE 1
  //
  // Use `IO.fromFuture` method to convert the following `Future` into an `IO`.
  //
  val future1 = () => Future.successful("Hello World")
  val io1: IO[Throwable, String] = IO.fromFuture(???)(global)

  //
  // EXERCISE 2
  //
  // Use the `toFuture` method on `IO` to convert the following `io` to `Future`.
  //
  val io2: IO[Throwable, Int] = IO.point(42)
  val future2: IO[Nothing, Future[Int]] = io2 ?

  //
  // EXERCISE 3
  //
  // Use the Fiber.fromFuture` method to convert the following `Future` into
  // an `IO`.
  //
  val future3 = () => Future.failed[Int](new Error("Uh ohs!"))
  val fiber1: Fiber[Throwable, Int] = Fiber.fromFuture(???)(global)

  import scalaz.zio.interop.Task
  import scalaz.zio.interop.catz._
  import cats.effect.concurrent.Ref

  //
  // EXERCISE 4
  //
  // The following example uses the `Ref` from `cats-effect`, demonstrating how
  // `cats-effect` structures work with ZIO.
  //
  class Worker(number: Int, ref: Ref[Task, Int]) {
    def work: Task[Unit] =
      for {
        c1 <- ref.get
        _  <- putStrLn(s"#$number >> $c1")
        c2 <- ref.modify(x => (x + 1, x))
        _  <- putStrLn(s"#$number >> $c2")
      } yield ()
  }

  val program: Task[Unit] =
    for {
      ref <- Ref.of[Task, Int](0)
      w1  = new Worker(1, ref)
      w2  = new Worker(2, ref)
      w3  = new Worker(3, ref)
      f   <- IO.forkAll(List(w1.work, w2.work, w3.work))
      _   <- f.join
    } yield ()
}

object zio_rts {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Create a new runtime system (that extends scalaz.zio.RTS).
  //
  val MyRTS: RTS = ???

  //
  // EXERCISE 2
  //
  // Run the following `IO` by using the `unsafeRun` method of `MyRTS`.
  //
  (putStrLn("Hello World") ? : Unit)

  //
  // EXERCISE 3
  //
  // Run the following `IO` by using the `unsafeRunSync` method of `MyRTS`.
  //
  import java.io.IOException
  (putStrLn("Hello World") ? : ExitResult[IOException, Unit])

  //
  // EXERCISE 4
  //
  // In this purely functional main program (made possible by `App`), ask the
  // user for their name and print it out again.
  //
  object MyApp extends App {
    def run(args: List[String]): IO[Nothing, ExitStatus] =
      (for {
        _ <- putStrLn("Hello World!")
      } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
  }
}

object zio_advanced {

}
