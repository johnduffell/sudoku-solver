# sudoku-solver
This only does two types of move
1. any cell that's known, remove that value as a possibility for all other cells in the same zone (row, column, block)
1. any value that's only possible in a single position in a zone, remove all the other possibilities from that cell
1. any trio (three cells that are in the same block and row or column) where a value can't be elsewhere in the block, it must be in those three squares, so remove the possibility from elsewhere in the row or column. (generalisation of case 2)
1. any zone where there are n cells where between them all there are only n values possible, those n values aren't possible in any other cell in that zone (generalisation of case 1)

This seemed to solve the very hard from http://www.sudokukingdom.com/
But I am sure there are some it can't do.

It wouldn't do Hidden Subset from https://www.kristanix.com/sudokuepic/sudoku-solving-techniques.php that would be a generalisation of case 2, any N values that are only possible in N squares, regardless of any other values possible in those fields, remove the others.

It can't handle any that need the XY wing technique http://www.sudokuessentials.com/Sudoku-XY-Wing.html

In the 4th case it needs one of the cells to have all the possibilities, it doesn't notice where we have a 1,2 2,3 3,1 in the same zone and remove the 1,2,3 from all other cells in the zone
