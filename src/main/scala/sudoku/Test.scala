package sudoku

object Test extends App {

  import Sudoku._

  val initial = boardFromStrings(
    List(
//      "36   2 1 ",
//      "   3    5",
//      "9  7   6 ",
//      "24 86    ",
//      "      628",
//      " 16   3  ",
//      " 596 7   ",
//      "    851  ",
//      "  2 4 5 7"
//      "     72  ",
//      " 53    1 ",
//      "    15  3",
//      " 7  2   9",
//      "5  6   8 ",
//      "        6",
//      " 3 2 8   ",
//      "4 1   5  ",
//      "2   9  7 "
      "  8 74   ",
      " 2     8 ",
      " 9  3   7",
      "8 596    ",
      "   2  1  ",
      "      53 ",
      "     136 ",
      "14   6   ",
      "9  5     "
    )
  )

  println(s"board\n$initial")
  val display = boardToString(initial)
  println(s"board\n$display")
  val solved = Solver.solve(initial, 32, {case (desc, board) => println(s"debug - $desc:\n${boardToString(board)}")})
  println(s"\n\nsolved:\n\n${boardToString(solved)}")

  def boardFromStrings(lines: List[String]): Board = {

    def rowFromString(line: String): UniqueZone = {

      def cellFromChar(char: Char): Cell = {
        char match {
          case ' ' => Cell(allValues.toSet)
          case other => Cell(Set(other match {
            case '1' => S1
            case '2' => S2
            case '3' => S3
            case '4' => S4
            case '5' => S5
            case '6' => S6
            case '7' => S7
            case '8' => S8
            case '9' => S9
          }))
        }
      }

      UniqueZone.fromList(line.toCharArray.toList.map(cellFromChar))
    }

    lines.map(rowFromString) match {
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: Nil =>
        Board(a, b, c, d, e, f, g, h, i)
    }
  }

  def boardToString(board: Board): String = {

    def rowToString(row: UniqueZone): String = {

      def cellToString(cell: Cell): (String, String, String) = {

        def valueToString(value: Sudoku.Value): String = {
          value match {
            case S1 => "1"
            case S2 => "2"
            case S3 => "3"
            case S4 => "4"
            case S5 => "5"
            case S6 => "6"
            case S7 => "7"
            case S8 => "8"
            case S9 => "9"
          }
        }

        cell match {
          case Cell(possibilities) if !cell.isKnown => allValues.map(v => if (possibilities.contains(v)) valueToString(v) else " ").grouped(3).map(_.mkString).toList match {
            case a1 :: a2 :: a3 :: Nil => (a1, a2, a3)
          }
          case cell => {
            val num = valueToString(cell.maybeKnown.get)
            ("+-+", s"|$num|", "+-+")
          }
        }
      }

      val unsequenced = List(cellToString(row.c1),
        cellToString(row.c2),
        cellToString(row.c3),
        ("|", "|", "|"),
        cellToString(row.c4),
        cellToString(row.c5),
        cellToString(row.c6),
        ("|", "|", "|"),
        cellToString(row.c7),
        cellToString(row.c8),
        cellToString(row.c9)
      )
        List(unsequenced.map(_._1),
          unsequenced.map(_._2),
          unsequenced.map(_._3)).map(_.mkString(" ")).mkString("\n")
    }

    List(rowToString(board.r1),
      rowToString(board.r2),
      rowToString(board.r3),
      "------------+-------------+------------",
      rowToString(board.r4),
      rowToString(board.r5),
      rowToString(board.r6),
      "------------+-------------+------------",
      rowToString(board.r7),
      rowToString(board.r8),
      rowToString(board.r9)
    ).mkString("\n\n")
  }

}
