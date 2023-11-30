{- Game logic / win checking -}
module Game(module Game) where

import Data.Maybe (isJust)
import Control.Monad (msum)

{- Board and counters definition -}
{- In our case, we represent a board as a function from column IDs to lists of tokens.
 - These are sparse and grow upwards. -}
{- Row indices again begin at the bottom left and grow upwards -} 
type RowID = Int
type ColumnID = Int
type RowCount = Int
type ColCount = Int
data Player = Red | Yellow
    deriving (Eq)

data Board = MkBoard { board :: [[Player]], numRows :: Int, numCols :: Int }

{- Toggles the current player -}
togglePlayer :: Player -> Player
togglePlayer Red = Yellow
togglePlayer Yellow = Red

{- Board accessors / manipulation function -}

{- Q1(a): emptyBoard -}
emptyBoard :: RowCount -> ColCount -> Board
emptyBoard rowCount colCount
    | rowCount <= 0 || colCount <= 0 = error "Invalid dimensions"
    | otherwise = MkBoard { board = replicate colCount [], numRows = rowCount, numCols = colCount }

{- Q1(b): getCounter
 - Gets the counter at the given co-ordinates (or Nothing if there is no counter there).
 - Raises an error if co-ordinates are out-of-bounds. -}
getCounter :: Board -> RowID -> ColumnID -> Maybe Player
getCounter boardField rowId colId
    | colId < 0 || colId >= numCols boardField = error "Column out of bounds"
    | rowId < 0 || rowId >= numRows boardField = error "Row out of bounds"
    | otherwise = case (board boardField !! colId) of
        column -> if rowId < length column then Just (column !! rowId) else Nothing


{- Q1(c): getRow
 - Retrieves the list of counters on the given row -}
getRow :: Board -> RowID -> [Maybe Player]
getRow boardField rowId
    | rowId < 0 || rowId >= numRows boardField = error "Row out of bounds"
    | otherwise = map (\column -> if rowId < length column then Just (column !! rowId) else Nothing) (board boardField)



{- Q1(d): getColumn
 - Retrieves the list of counters in the given column, from top-to-bottom -}
getColumn :: Board -> ColumnID -> [Maybe Player]
getColumn boardField colId
    | colId < 0 || colId >= numCols boardField = error "Column out of bounds"
    | otherwise = let col = board boardField !! colId
                      padding = replicate (numRows boardField - length col) Nothing
                  in map Just (reverse col) ++ padding
{- Q2: Show instance -}

{- Show instance for players -}
instance Show Player where
    show Red = "R"
    show Yellow = "Y"

{- Instance -}
instance Show Board where
 show boardField = unlines . reverse . map showRow $ [0 .. numRows boardField - 1]
        where showRow r = map (maybe 'O' playerToChar) (getRow boardField r)
              playerToChar player = head (show player)

{- Q3: Board update -}

{- Drops a counter into the given column. If the move is legal, returns an updated
 - board. Otherwise returns Nothing. -}
dropCounter :: Board -> ColumnID -> Player -> Maybe Board
dropCounter boardField colId player
    | colId < 0 || colId >= numCols boardField = Nothing
    | isColumnFull (board boardField !! colId) = Nothing
    | otherwise = Just (boardField { board = updatedBoard })
    where
        updatedBoard = take colId (board boardField) ++ [updatedColumn] ++ drop (colId + 1) (board boardField)
        updatedColumn = addCounterToColumn player (board boardField !! colId)

        isColumnFull column = length column >= numRows boardField

        addCounterToColumn p column
            | length column < numRows boardField = column ++ [p]
            | otherwise = column



{- Q4: Diagonals -}
getTLBRDiagonals :: Board -> [[Maybe Player]]
getTLBRDiagonals boardField = [getDiagonal boardField rowId colId | rowId <- [0 .. numRows boardField - 1], colId <- [numCols boardField - 1]] ++
                     [getDiagonal boardField rowId colId | rowId <- [numRows boardField - 1], colId <- [0 .. numCols boardField - 2]]
    where
        getDiagonal :: Board -> RowID -> ColumnID -> [Maybe Player]
        getDiagonal boardField rowId colId
            | rowId < 0 || colId < 0 = []
            | otherwise = getCounter boardField rowId colId : getDiagonal boardField (rowId - 1) (colId - 1)


getBLTRDiagonals :: Board -> [[Maybe Player]]
getBLTRDiagonals boardField = [getDiagonal boardField rowId colId | rowId <- [0 .. numRows boardField - 1], colId <- [0]] ++
                     [getDiagonal boardField rowId colId | rowId <- [0], colId <- [1 .. numCols boardField - 1]]
    where
        getDiagonal :: Board -> RowID -> ColumnID -> [Maybe Player]
        getDiagonal boardField rowId colId
            | rowId >= numRows boardField || colId >= numCols boardField = []
            | otherwise = getCounter boardField rowId colId : getDiagonal boardField (rowId + 1) (colId + 1)


{- Q5: Win checking -}
{- Checks if the given list has a subsequence of length 4, returning Just Player
 - if so, Nothing otherwise -}
hasFourInRow :: [Maybe Player] -> Maybe Player
hasFourInRow (a:b:c:d:rest)
    | a == b && b == c && c == d && isJust a = a
    | otherwise = hasFourInRow (b:c:d:rest)
hasFourInRow _ = Nothing


{- Checks all rows, columns, and diagonals for any subsequences of length 4 -}
checkWin :: Board -> Maybe Player
checkWin boardField = msum $ map hasFourInRow sequences
    where
        sequences = [getRow boardField rowId | rowId <- [0 .. numRows boardField - 1]] ++
                    [getColumn boardField colId | colId <- [0 .. numCols boardField - 1]] ++
                    getTLBRDiagonals boardField ++
                    getBLTRDiagonals boardField

