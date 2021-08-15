module Rendering where

import Graphics.Gloss
import Data.Array 

import Game

boardGridColor = makeColorI 255 255 255 255
playerXColor = makeColorI 255 50 50 255
playerOColor = makeColorI 50 100 255 255
tieColor = greyN 0.5

boardAsRunningPicture board = 
    pictures [ color playerXColor $ xCellsOfBoard board 
             , color playerOColor $ oCellsOfBoard board
             , color boardGridColor boardGrid 
             ]

--get the outcome colour depending on the winner
outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row,column) = translate x y picture 
    where x = fromIntegral column * cellWidth + cellWidth * 0.5 --x and y coordinates depending on the side of the cell
          y = fromIntegral row * cellHeight + cellHeight * 0.5

--create the X symbol
xCell :: Picture
xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0 
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]   
    where side = min cellWidth cellHeight * 0.75

oCell :: Picture 
oCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

--helper function to return a picture will the player tokens in the correct position
cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures --combine all the pictures into a single picture
    $ map (snapPictureToCell cellPicture . fst) --add the coordinates to the cellPicture
    $ filter (\(_,e)-> e == cell) --only get the elements with something in a cell
    $ assocs board --get the 2d board array [((0,0),Empty),((0,1),Empty)... as a list

xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Full PlayerX) xCell 

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Full PlayerO) oCell 

boardGrid :: Picture
boardGrid = 
    pictures
    --create horizontal and verticle lines to represent the grid
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)                         --verticle line
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)   --horizontal line
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n] -- =[0.0,1.0,2.0,3.0]

boardAsPicture board = 
    pictures [ xCellsOfBoard board
             , oCellsOfBoard board 
             , boardGrid
             ]

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
--have to translate to make sure board is rendered from the tl corner and not the center
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5)) 
                               ( fromIntegral screenHeight * (-0.5)) 
                               frame
    where frame = case gameState game of
            Running -> boardAsRunningPicture (gameBoard game) --pass in the board of the game
            GameOver winner -> boardAsGameOverPicture winner (gameBoard game)
