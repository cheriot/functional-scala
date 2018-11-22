package net.degoes

import scalaz.zio._
import scalaz.zio.console._

object FunctionalScala extends App {

  sealed trait Side
  case object X extends Side
  case object O extends Side

  case class TicTacBoard private (val board: List[List[Option[Side]]])
  object TicTacBoard {
    def apply(): TicTacBoard = TicTacBoard(
      List.fill(3)(
        List.fill(3)(None)
      )
    )
  }

  import scalaz.zio._
  import scalaz.zio.interop.scalaz72._
  import scalaz._
  import Scalaz._
  def enterValidPosition(msg: String): IO[Throwable, Int] =
    for {
      _ <- putStrLn(msg)
      input <- getStrLn
      pick <- scala.util.Try(input.toInt).toOption match {
        case Some(num) if (1 to 3).contains(num) => IO.point(num)
        case _ => putStrLn(s"$input is not a valid number between 1 and 3.") *> enterValidPosition(msg)
      }
    } yield pick


  def isWin(board: TicTacBoard) : Boolean =
    {
      val rows = board.board
      val cols = (0 until 3).map(i => board.board.map(col => col.lift(i)))
      ???
    }

  def isValidMove(row: Int, col: Int, board: TicTacBoard): Boolean =
    (
      for {
        rowList <- board.board.lift(row)
        _ <- rowList.lift(col)
      } yield true
    ).getOrElse(false)

  def renderBoard(board: TicTacBoard) : IO[Nothing, Unit] = ???

  def gameLoop(board: TicTacBoard, nextPlay: Side): IO[Throwable, TicTacBoard] =
    for {
      _ <- renderBoard(board)
      row <- enterValidPosition(s"Player $nextPlay, pick a row.")
      col <- enterValidPosition("Now pick a column.")
      nextBoard <- if (!isValidMove(row, col, board))
                     putStrLn(s"($row,$col) is already taken. Please pick an empty space") *>
                       gameLoop(board, nextPlay)
                   else {
                     IO.point(board)
                   }
    } yield nextBoard

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Read? Tic Tac Go!")
      board = TicTacBoard()
      _ <- gameLoop(board, X)
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
