package sudoku

import cats._
import cats.implicits._

object Sudoku {

  val traverse = implicitly[Traverse[List]]

  sealed trait Value

  case object S1 extends Value

  case object S2 extends Value

  case object S3 extends Value

  case object S4 extends Value

  case object S5 extends Value

  case object S6 extends Value

  case object S7 extends Value

  case object S8 extends Value

  case object S9 extends Value

  val allValues: List[Value] = List(S1, S2, S3, S4, S5, S6, S7, S8, S9)

  case class Cell(possibilities: Set[Value]) {
    def isKnown: Boolean = possibilities.size == 1
    def maybeKnown: Option[Value] = if (isKnown) Some(possibilities.head) else None
  }

  case class UniqueZone(
    c1: Cell,
    c2: Cell,
    c3: Cell,
    c4: Cell,
    c5: Cell,
    c6: Cell,
    c7: Cell,
    c8: Cell,
    c9: Cell
  ) {
    val firstTrio = List(c1, c2, c3)
    val middleTrio = List(c4, c5, c6)
    val lastTrio = List(c7, c8, c9)
    val trios = List(firstTrio, middleTrio, lastTrio)
    val cells = trios.flatten


    def map(f: Cell => Cell): UniqueZone =
      UniqueZone.fromList(cells.map(f))

    def mapTrios[M[_] : Monad](f: (List[Cell], Int) => M[List[Cell]]): M[UniqueZone] = {

      val monad = implicitly[Monad[M]]

      val rrrr = trios.zipWithIndex.map({case (v, index) => f(v, index)})

      traverse.sequence(rrrr).map(_.flatten).map(UniqueZone.fromList)
    }

  }

  object UniqueZone {
    def fromList(rows: List[Cell]): UniqueZone = {
      rows match {
        case a :: b :: c :: d :: e :: f :: g :: h :: i :: Nil =>
          UniqueZone(a, b, c, d, e, f, g, h, i)
      }
    }
  }

  case class Board(
    r1: UniqueZone,
    r2: UniqueZone,
    r3: UniqueZone,
    r4: UniqueZone,
    r5: UniqueZone,
    r6: UniqueZone,
    r7: UniqueZone,
    r8: UniqueZone,
    r9: UniqueZone
  ) {
    val top = List(r1, r2, r3)
    val middle = List(r4, r5, r6)
    val bottom = List(r7, r8, r9)
    val rows = top ++ middle ++ bottom

    val columns: List[UniqueZone] = List(
      rows.map(_.c1),
      rows.map(_.c2),
      rows.map(_.c3),
      rows.map(_.c4),
      rows.map(_.c5),
      rows.map(_.c6),
      rows.map(_.c7),
      rows.map(_.c8),
      rows.map(_.c9)
    ).map(UniqueZone.fromList)

    val blocks: List[UniqueZone] = {
      def getHorizontallyAdjacentBlocksTrio(threeRows: List[UniqueZone]): List[UniqueZone] = {
        val tl = threeRows.flatMap(_.firstTrio)
        val tm = threeRows.flatMap(_.middleTrio)
        val tr = threeRows.flatMap(_.lastTrio)
        List(tl, tm, tr).map(UniqueZone.fromList)
      }

      val topRows = getHorizontallyAdjacentBlocksTrio(top)
      val middleRows = getHorizontallyAdjacentBlocksTrio(middle)
      val bottomRows = getHorizontallyAdjacentBlocksTrio(bottom)

      topRows ++ middleRows ++ bottomRows
    }

    val blocksVertical: List[UniqueZone] = {
      def getHorizontallyAdjacentBlocksTrio(threeRows: List[UniqueZone]): List[UniqueZone] = {
        val tl = threeRows.flatMap(_.firstTrio)
        val tm = threeRows.flatMap(_.middleTrio)
        val tr = threeRows.flatMap(_.lastTrio)
        List(tl, tm, tr).map(UniqueZone.fromList)
      }

      columns.grouped(3).toList match {
        case leftThick :: middleThick :: rightThick :: Nil =>
        val topRows = getHorizontallyAdjacentBlocksTrio(leftThick)
        val middleRows = getHorizontallyAdjacentBlocksTrio(middleThick)
        val bottomRows = getHorizontallyAdjacentBlocksTrio(rightThick)

        topRows ++ middleRows ++ bottomRows
      }
    }

    def mapZone[M[_] : Monad](zones: List[UniqueZone], f: UniqueZone => M[UniqueZone], from: List[UniqueZone] => Board): M[Board] = {

      val monad = implicitly[Monad[M]]

      val mapped = zones.map(f)
      monad.map(traverse.sequence(mapped))(from)
      //        val (count, newRows) = mapped.foldRight((0, List[UniqueZone]())) {
      //          case ((newCount, newRow), (prevCount, rowsSoFar)) =>
      //            (prevCount + newCount, newRow :: rowsSoFar)
      //        }
      //        val board = from(newRows)
      //        (count, board)
    }

    def mapRows[M[_] : Monad](f: UniqueZone => M[UniqueZone]): M[Board] =
      mapZone(rows, f, Board.fromRowsList)

    def mapColumns[M[_] : Monad](f: UniqueZone => M[UniqueZone]): M[Board] =
      mapZone(columns, f, Board.fromColumnsList)

    def mapBlocks[M[_] : Monad](f: UniqueZone => M[UniqueZone]): M[Board] =
      mapZone(blocks, f, Board.fromBlocksList)

    def mapAll[M[_] : Monad](f: UniqueZone => M[UniqueZone]): M[Board] = {

      val monad = implicitly[Monad[M]]

      for {
        newRows <- mapRows(f)
        newCol <- newRows.mapColumns(f)
        newBlock <- newCol.mapBlocks(f)
      } yield newBlock
    }

  }

  object Board {
    def fromRowsList(rows: List[UniqueZone]): Board = {
      rows match {
        case a :: b :: c :: d :: e :: f :: g :: h :: i :: Nil =>
          Board(a, b, c, d, e, f, g, h, i)
      }
    }

    def fromColumnsList(columns: List[UniqueZone]): Board = {
      Board(
        UniqueZone.fromList(columns.map(_.c1)),
        UniqueZone.fromList(columns.map(_.c2)),
        UniqueZone.fromList(columns.map(_.c3)),
        UniqueZone.fromList(columns.map(_.c4)),
        UniqueZone.fromList(columns.map(_.c5)),
        UniqueZone.fromList(columns.map(_.c6)),
        UniqueZone.fromList(columns.map(_.c7)),
        UniqueZone.fromList(columns.map(_.c8)),
        UniqueZone.fromList(columns.map(_.c9))
      )
    }

    def fromBlocksList(blocks: List[UniqueZone]): Board = {
      Board.fromRowsList(
        blocks.grouped(3).toList.flatMap { blocksInTop =>
          List(
            blocksInTop.flatMap(_.firstTrio),
            blocksInTop.flatMap(_.middleTrio),
            blocksInTop.flatMap(_.lastTrio)
          ).map(UniqueZone.fromList)
        }
      )

    }

    def fromBlocksVerticalList(blocks: List[UniqueZone]): Board = {
      Board.fromColumnsList(
        blocks.grouped(3).toList.flatMap { blocksInTop =>
          List(
            blocksInTop.flatMap(_.firstTrio),
            blocksInTop.flatMap(_.middleTrio),
            blocksInTop.flatMap(_.lastTrio)
          ).map(UniqueZone.fromList)
        }
      )

    }

  }

}



