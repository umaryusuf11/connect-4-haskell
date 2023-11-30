module Render(module Render) where
import Game (board, dropCounter, Board, Player(Red, Yellow), numRows, numCols, emptyBoard, checkWin, togglePlayer)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Interact


import Data.Maybe (isJust)
type GameState = (Board, Player, Maybe Player, Bool) -- Represents (board, current_player, winner, waiting_for_reset)


{- Viewport dimensions -}
windowWidth :: Float
windowWidth = 1024.0

windowHeight :: Float
windowHeight = 768.0

-- Q6 (Rendering the board)
render :: Board -> Picture
render board = pictures [renderGrid board, renderCounters board]

renderGrid :: Board -> Picture
renderGrid board = pictures $ concat [horizontalLines, verticalLines]
  where
    cellWidth = windowWidth / fromIntegral (numCols board)
    cellHeight = windowHeight / fromIntegral (numRows board)

    horizontalLines = [color blue (line [(-windowWidth / 2, y), (windowWidth / 2, y)]) | y <- [(-windowHeight / 2), (-windowHeight / 2) + cellHeight .. (windowHeight / 2)]]
    verticalLines = [color blue (line [(x,-windowHeight / 2), (x, windowHeight / 2)]) | x <- [(-windowWidth / 2), (-windowWidth / 2) + cellWidth .. (windowWidth / 2)]]

renderCounters :: Board -> Picture
renderCounters boardField = pictures [translate x y (color col (circleSolid radius)) |
                                        (colId, column) <- zip [0 .. ] (board boardField),
                                        (rowId, player) <- zip [0 .. ] column,
                                        let x = fromIntegral colId * cellWidth - windowWidth / 2 + cellWidth / 2,
                                        -- Calculate the y position, starting from the bottom of the window
                                        let y = -windowHeight / 2 + fromIntegral rowId * cellHeight + cellHeight / 2,
                                        let col = getColor player]
  where
    cellWidth = windowWidth / fromIntegral (numCols boardField)
    cellHeight = windowHeight / fromIntegral (numRows boardField)
    radius = min cellWidth cellHeight / 3 

    getColor :: Player -> Color
    getColor Red = red
    getColor Yellow = yellow

renderGame :: GameState -> IO Picture
renderGame (boardField, _, maybeWinner, waitingForReset) =
    return $ pictures [renderGrid boardField, renderCounters boardField, winMessage maybeWinner, resetMessage waitingForReset]
    where
        winMessage (Just winner) = displayMessage (playerWinsMessage winner) (windowHeight / 4) -- Position for win message
        winMessage Nothing       = Blank

        resetMessage True = displayMessage "Press 'R' to reset" (windowHeight / 6) -- Position for reset message
        resetMessage False = Blank

-- Q7 (Game loop)
-- Example of how to use displayIO (similar to interactIO)
testPicture :: IO ()
testPicture =
    displayIO
        (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
        white
        (return (color red (rectangleSolid 50 50)))
        (const $ return ())


gameLoop :: Int -> Int -> IO ()
gameLoop rowCount colCount = do
    let gameBoard = emptyBoard rowCount colCount
    interactIO
        (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
        white
        (gameBoard, Red, Nothing, False)
        renderGame 
        handleEvent
        update
    where
        handleEvent :: Event -> GameState -> IO GameState
        handleEvent event (boardField, player, maybeWinner, waitingForReset) =
            case event of
                -- Handle 'R' key press for resetting the game
                EventKey (Char 'r') Up _ _ ->
                    if waitingForReset
                    then return (emptyBoard (numRows boardField) (numCols boardField), Red, Nothing, False) -- Reset the game
                    else return (boardField, player, maybeWinner, waitingForReset) -- No reset if the game is not waiting for it

                -- Handle mouse clicks for game moves
                EventKey (MouseButton LeftButton) Up _ (mouseX, _) ->
                    if not waitingForReset then
                        let colId = floor ((mouseX + windowWidth / 2) / (windowWidth / fromIntegral (numCols boardField)))
                        in case dropCounter boardField colId player of
                            Just boardField' ->
                                let winner = checkWin boardField' in
                                if isJust winner
                                then return (boardField', player, winner, True) -- Winner found, set waitingForReset
                                else return (boardField', togglePlayer player, Nothing, False) -- Continue game, no winner
                            Nothing -> return (boardField, player, Nothing, False) -- Invalid move
                    else 
                        return (boardField, player, maybeWinner, waitingForReset) -- Ignore clicks if waiting for reset

                -- Default case for other events
                _ -> return (boardField, player, maybeWinner, waitingForReset)




        update :: Controller -> IO ()
        update _ = return ()


playerWinsMessage :: Player -> String
playerWinsMessage player = show player ++ " wins!"

displayMessage :: String -> Float -> Picture
displayMessage message yPos = translate (-windowWidth / 4) yPos $ scale 0.3 0.3 $ color black $ Text message
