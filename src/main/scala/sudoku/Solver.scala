package sudoku

import cats._
import cats.data.{Writer, WriterT}
import cats.implicits._
import sudoku.Sudoku.{Board, Cell, UniqueZone, _}

object Solver {

  type Counter[A] = Writer[Int, A]

  implicit val monad = implicitly[Monad[Counter]](
    WriterT.catsDataCommutativeMonadForWriterT(
      catsInstancesForId, catsKernelStdGroupForInt
    )
  )
  val traverse = implicitly[Traverse[List]]

  // anything that's known for sure in this zone, remove from all other possibilities
  def filterWithinZone(cells: List[Cell]): Counter[List[Cell]] = {
    val knownInRow: Set[Value] = cells.flatMap(_.maybeKnown).toSet
    traverse.sequence(cells.map {
      case cell @ Cell(possibilities) if !cell.isKnown =>
        val newPoss = possibilities.diff(knownInRow)
        val add = if (newPoss.size < possibilities.size) 1 else 0
        val updatedCell: Cell = newPoss.toList match {
          case Nil => throw new RuntimeException(s"EEKKE")
          case _ => Cell(newPoss)
        }
        Writer(add, updatedCell)
      case cell => Writer(0, cell)
    })

  }

  // any number that can only occur in one place must be known
  def noticeSingleOptions(cells: List[Cell]): Counter[List[Cell]] = {
    val unknownInRow: Set[Value] = allValues.toSet.diff(cells.flatMap(_.maybeKnown).toSet)
    val occurrencesPerUnknownSymbol = cells.foldLeft(Map[Value, Int]()){
      case (counts, Cell(possibilities)) =>

        possibilities.foldLeft(counts) { case (counts, possibility) =>
          if (unknownInRow.contains(possibility))
            counts.updated(possibility, counts.getOrElse(possibility, 0) + 1)
          else
            counts
        }
    }

    val newlyKnownValues = occurrencesPerUnknownSymbol.toList.collect({
      case (_, 0) => throw new RuntimeException("errrr!")
      case (value, 1) => value
    }).toSet

    traverse.sequence(cells.map { cell =>
      val found = cell.possibilities.intersect(newlyKnownValues)
      if (found.size > 1) throw new RuntimeException("hahaha")
      if (found.size == 1)
        Writer(1, Cell(found))
      else
        Writer(0, cell)
    })

  }

  // if a number is known down to a trio in a block horizontally or vertically, remove it from all other boxes in the column/row
  def filterWithinZoneIfKnownWithinBlockTrio(blocksToCheck: List[UniqueZone], rowsToUpdate: List[UniqueZone]): Counter[List[UniqueZone]] = {
    //make a map from numbers to possible trios
    blocksToCheck.zipWithIndex.foldLeft(Writer(0, rowsToUpdate)) {
      case (rowsToUpdateCounter, (blockToCheck, blockNum)) =>
        val cells = blockToCheck.cells
//        val unknownInBlock: Set[Value] = allValues.toSet.diff(cells.flatMap(_.maybeKnown).toSet)

        val symbolsToTrios = blockToCheck.trios.zipWithIndex.foldLeft(Map[Value, Either[Boolean, Int]]()){
          case (counts, (cellsInTrio, trioNum)) =>
            val possibilitiesInTrio = cellsInTrio.flatMap(_.possibilities).toSet

            possibilitiesInTrio.foldLeft(counts) { case (counts, possibility) =>
              counts.getOrElse(possibility, Left(false)) match {//false is none found yet
                case Left(false) =>
                  counts.updated(possibility, Right(trioNum))
                case Right(_) =>
                  counts.updated(possibility, Left(true))//true is multiple
                case _ =>
                  counts
              }

            }
        }.toList.collect {
          case (value, Right(whichTrio)) => (value, whichTrio)
        }
        val triosToSymbolsOnlyInThatOne = symbolsToTrios.groupBy(_._2).mapValues(_.map(_._1).toSet)

        // if it's blockNum 0, 3, 6 we need to remove from the later 2/3rds of the row
        val whichSetOfThreeRows = blockNum / 3 // 0-2 inclusive
        val whichColumnInTheThickRowNotRemove = blockNum % 3 // 0-2 inclusive
        rowsToUpdateCounter.flatMap { rowsToUpdateSoFar =>
          val rowsWithTrioNumber = rowsToUpdateSoFar.grouped(3).toList.zipWithIndex.map { case (v, index) => v.zipWithIndex.map { case (v, subindex) => (v, index, subindex) } }.flatten
          traverse.sequence(rowsWithTrioNumber.map {
            case (row, topMiddleOrBottomInt, withinTrioNum) if (topMiddleOrBottomInt == whichSetOfThreeRows) =>
              // go through the row based on any in triosToSymbols that match withinTrioNum but don't remove from tonotRemove
              val valuesOnlyInWithinTrioNumPosition = triosToSymbolsOnlyInThatOne.getOrElse(withinTrioNum, Set())
              row.mapTrios[Counter] {
                case (trio, index) =>
                  if (index == whichColumnInTheThickRowNotRemove)
                    Writer(0, trio)
                  else
                    traverse.sequence(trio.map { cell =>
                      // remove all valuesOnly from cell possibilities
                      val newPossibilities = cell.possibilities.diff(valuesOnlyInWithinTrioNumPosition)
                      if (newPossibilities.size < cell.possibilities.size) {
                        println(s"removed something in: ${valuesOnlyInWithinTrioNumPosition} using $blockNum from row $withinTrioNum within thick row $whichSetOfThreeRows")
                        println(s"cell.possibilities is ${cell.possibilities}")
                        println(s"newPossibilities is $newPossibilities")
                        Writer(1, Cell(newPossibilities))
                      } else
                        Writer(0, cell)
                    })
              }
            case (rowCurrentBlockDoesntAffect, _, _) =>
              Writer(0, rowCurrentBlockDoesntAffect)
          })
        }
    }
  }

  def filterWithinZoneAllGroupSizes(cells: List[Cell]): Counter[List[Cell]] = {
    (2 to 7).foldLeft(Writer(0, cells)) {
      case (cellsWriter, groupSize) =>
        cellsWriter.flatMap { cells =>
          filterWithinZoneN(cells, groupSize)
        }
    }
  }

  // if n numbers can only occur in n cells, remove those from all other cells in the zone
  // the one number can only occur in one cell is already taken care of
  def filterWithinZoneN(cells: List[Cell], n: Int): Counter[List[Cell]] = {

    def cellIsPartOfCycle(cell: Cell, knownGroupsInZone: Set[Set[Value]]): Boolean = {
      val possibilities = cell.possibilities
      knownGroupsInZone.exists { knownGroup =>
        possibilities.subsetOf(knownGroup)
      }
    }

    val knownGroupsInZone: Set[Set[Value]] = {
      val isNLong = cells
        .filter(cell => cell.possibilities.size == n)

      val knownGroups = isNLong.groupBy(_.possibilities).keySet.map { cycleNValues =>
        val numberOfSubsets = cells.count(_.possibilities.subsetOf(cycleNValues))
        (cycleNValues, numberOfSubsets)
      }.toMap.filter(_._2 == n).keySet

      knownGroups
    }
    val allKnown = knownGroupsInZone.flatten
    traverse.sequence(cells.map {
      case cell @ Cell(possibilities) if !cellIsPartOfCycle(cell, knownGroupsInZone) =>
        val newPoss = possibilities.diff(allKnown)
        val add = if (newPoss.size < possibilities.size) 1 else 0
        val updatedCell: Cell = newPoss.toList match {
          case Nil => throw new RuntimeException(s"EEKKE")
          case _ => Cell(newPoss)
        }
        Writer(add, updatedCell)
      case cell => Writer(0, cell)
    })

  }

  def liftCellsToZone[M[_]: Monad](f2: List[Cell] => M[List[Cell]]): UniqueZone => M[UniqueZone] = { r =>
    val mCells = f2(r.cells)
    val monad = implicitly[Monad[M]]
    monad.map(mCells)(UniqueZone.fromList)
  }

  def solve(initial: Board, tries: Int, debug: (String, Board) => Unit): Board = {

    val (updated, iteratedBoard) = (for {
      filtered <- initial.mapAll[Counter] { zone =>
        liftCellsToZone(filterWithinZone)(monad)(zone)
      }
      singled <- filtered.mapAll[Counter] { zone =>
        liftCellsToZone(noticeSingleOptions)(monad)(zone)
      }
      trioFilteredHorizontal <- filterWithinZoneIfKnownWithinBlockTrio(singled.blocks, singled.rows)
        .map(Board.fromRowsList)

      trioFiltered <- filterWithinZoneIfKnownWithinBlockTrio(trioFilteredHorizontal.blocksVertical, trioFilteredHorizontal.columns)
        .map(Board.fromColumnsList)

      trioBlocksFilteredHorizontal <- filterWithinZoneIfKnownWithinBlockTrio(trioFiltered.rows, trioFiltered.blocks)
        .map(Board.fromBlocksList)

      trioBlocksFilteredVertical <- filterWithinZoneIfKnownWithinBlockTrio(trioBlocksFilteredHorizontal.columns, trioBlocksFilteredHorizontal.blocksVertical)
        .map(Board.fromBlocksVerticalList)

      filteredGroups <- trioBlocksFilteredVertical.mapAll[Counter] { zone =>
        liftCellsToZone(filterWithinZoneAllGroupSizes)(monad)(zone)
      }
    } yield filteredGroups).run

    debug("finished iteration", iteratedBoard)

    if (updated == 0) {
      println("terminating due to making no progress")
      iteratedBoard
    } else {
      if (tries == 0) {
        println("terminating due to run out of tries")
        iteratedBoard
      } else {
        solve(iteratedBoard, tries - 1, debug)
      }
    }
  }

}
