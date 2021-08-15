module Game where

import Data.Array


data Player = PlayerX | PlayerO deriving (Show,Eq)

data Cell = Empty | Full Player deriving (Show,Eq)
data State = Running | GameOver (Maybe Player) deriving (Eq,Show)

type Board = Array (Int, Int) Cell --the board is a 2d array of cells

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq,Show)

n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n


--make a board with cells from (0,0) to (2,2) which are all empty
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Empty)
                    , gamePlayer = PlayerX
                    , gameState = Running
                    }
            where indexRange = ((0,0),(n-1,n-1))




