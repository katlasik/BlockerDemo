package com.softwaremill

import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors

import cats.effect.IO

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.io.StdIn

package object softwaremill {

  def printLine(s: String): IO[Unit] =
    for {
      _ <- IO(println(s))
      _ <- showThread
    } yield ()

  def readLineBlocking: IO[String] = for {
    line <- IO(StdIn.readLine())
    _ <- showThread
  } yield line

  def getFileSizeBlocking(path: String): IO[Long] =
    for {
      path <- IO(Paths.get(path))
      size <- IO(Files.size(path))
      _ <- showThread
    } yield size

  def getFileLinesBlocking(path: String): IO[Long] =
    for {
      _ <- IO.delay(Thread.sleep(1000))
      path <- IO(Paths.get(path))
      lines <- IO(Files.lines(path).count())
    } yield lines

  def showThread: IO[Unit] =
    IO.delay(println(
      s"${Console.RED}<<Operation executed on ${Thread.currentThread().getName}>>${Console.RESET}"
    ))

  val BlockingEC: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(
    Executors.newCachedThreadPool((r: Runnable) => {
      val t = new Thread(r)
      t.setName(s"blocking-ec-${t.getName()}")
      t
    })
  )

}
