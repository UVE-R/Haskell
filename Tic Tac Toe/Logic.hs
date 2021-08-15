module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game

import Data.Array

isCoordCorrect = inRange ((0,0),(n-1,n-1))

--switch the current player
switchPlayer game =
    case gamePlayer game of
        PlayerX -> game {gamePlayer = PlayerO}
        PlayerO -> game {gamePlayer = PlayerX}

--check if any player has won
playerWon :: Player -> Board -> Bool
playerWon player board = any isVictoryProj projs
    where projs = allRowCoords
              ++ allColumnCoords
              ++ allDiagCoords
          allRowCoords = [[(i,j) | j <- [0.. n-1]] | i<- [0.. n-1]] -- =[[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)],[(2,0),(2,1),(2,2)]]
          allColumnCoords = [[(j,i) | j <- [0.. n-1]] | i<- [0.. n-1]]
          allDiagCoords = [[(i,i)| i <- [0 .. n-1]]
                       ,[(i,n-1-i) | i<- [0 .. n-1]]
                       ]
          isVictoryProj proj = (n ==) --check for n consecutive player counters
                            $ length --get the length
                            $ filter (\cell -> cell == Full player) --only get the player cells
                            $ map (board !) proj --new list of Empty and player cells

--count the number of non-empty cells
countCells :: Cell -> Board -> Int
countCells cell = length . filter(cell ==) . elems

checkGameOver game
    | playerWon PlayerX board =
        game {gameState = GameOver $ Just PlayerX}
    | playerWon PlayerO board =
        game {gameState = GameOver $ Just PlayerO}
    | countCells Empty board == 0 =
        game {gameState = GameOver Nothing}
    | otherwise = game
    where board = gameBoard game


playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Empty =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Full player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

--convert coordinates to cell row col
mousePosAsCellCoord :: (Float ,Float ) -> (Int,Int)
mousePosAsCellCoord (x,y) = ( floor((y + (fromIntegral screenHeight * 0.5)) / cellHeight) --need to add to shift to TL corner
                            , floor((x + (fromIntegral screenWidth  * 0.5))/ cellWidth )
                            )

transformGame (EventKey(MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> playerTurn game $ mousePosAsCellCoord mousePos
        GameOver _ -> initialGame --reinstate game to the start
transformGame _ game = game

