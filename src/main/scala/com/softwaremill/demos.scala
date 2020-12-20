package com.softwaremill

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.Executors

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import softwaremill._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object ShiftingDemo extends IOApp {

  val app: IO[Unit] = for {
    _ <- printLine("Enter path:")
    _ <- IO.shift(BlockingEC) //shifting to the blocking context
    path <- readLineBlocking
    lines <-
      getFileLinesBlocking(path).guarantee(
        IO.shift(contextShift)
      ) //shifting back must be done in finalizer
    _ <- printLine(s"File has $lines lines.")
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    app.as(ExitCode.Success)

}

object EvalOnDemo extends IOApp {

  //we combine two tasks to get single IO
  val readPathAndDisplaySize: IO[Long] =
    readLineBlocking.flatMap(getFileSizeBlocking(_))

  val app: IO[Unit] = for {
    _ <- printLine("Enter path:")
    //we run effect on blocking context and then right away shift back
    bytes <- contextShift.evalOn(BlockingEC)(readPathAndDisplaySize)
    _ <- printLine(s"File has $bytes bytes.")
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    app.as(ExitCode.Success)
}

object BlockerDemo extends IOApp {

  def checkIfExists(path: Path): IO[Boolean] = IO(Files.exists(path))

  def safeCreate(path: Path)(blocker: Blocker) =
    for {
      //we can block on io
      alreadyExists <- blocker.blockOn(checkIfExists(path))
      //or create effect running on blocking EC
      _ <-
        blocker.delay[IO, Unit](Files.createFile(path)).unlessA(alreadyExists)
    } yield ()

  //Blocker.apply returns resource which will automatically close underlying EC
  def app: IO[Unit] =
    Blocker[IO]
      .use { blocker =>
        for {
          _ <- printLine("Enter path:")
          line <- blocker.blockOn(readLineBlocking)
          path <- IO(Paths.get(line))
          _ <- safeCreate(path)(blocker)
        } yield ()
      }

  override def run(args: List[String]): IO[ExitCode] =
    app.as(ExitCode.Success)

}

object CustomBlockerDemo extends IOApp {

  val customBlockingEC: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(
      Executors.newCachedThreadPool((r: Runnable) => {
        val t = new Thread(r)
        t.setName(s"custom-blocking-ec-${t.getName()}")
        t
      })
    )

  def displayNameWithBlocker(blocker: Blocker): IO[Unit] =
    for {
      _ <- printLine("Enter your name:")
      name <- blocker.blockOn(readLineBlocking)
      _ <- printLine(s"Hello, $name")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    val blocker = Blocker.liftExecutionContext(customBlockingEC)

    displayNameWithBlocker(blocker)
      .as(ExitCode.Success)
      .guarantee(IO(customBlockingEC.shutdownNow()))
  }

}
