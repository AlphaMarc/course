module TicTacToeAPI
( Player
, GameResult
, EmptyBoard) where

import qualified Data.Map as M
import Data.Maybe (isJust)


data Player = Player1 | Player2
  deriving (Eq, Ord)

player1 :: Player
player1 = Player1

player2 :: Player
player2 = Player2

instance Show Player where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

isPlayer1 :: Player -> Bool
isPlayer1 Player1 = True
isPlayer1 _       = False

isPlayer2 :: Player -> Bool
isPlayer2 Player2 = True
isPlayer2 _       = False

toSymbol :: Player -> String
toSymbol Player1 = "X"
toSymbol Player2 = "O"


data GameResult = Win Player | Draw
   deriving (Show, Eq)

data Position = HG | HM | HD | MG | MM | MD | BG | BM | BD
  deriving (Eq, Ord, Show)

data EmptyBoard = EmptyBoard
    deriving (Show, Eq)

data Board = Board (M.Map Position Player)
    deriving (Eq, Show)

data FinishedBoard = FinishedBoard Board GameResult
   deriving (Show, Eq)

show :: Board -> String
show (Board m) = let pos p = maybe "?" toSymbol (M.lookup p m)
                 in concat [pos HG , "|" , pos HM , "|" , pos HD , "\n",
                            "-----\n",
                           pos MG , "|" , pos MM , "|" , pos MD , "\n",
                           "-----\n",
                           pos BG , "|" , pos BM , "|" ,  pos BD]




data MoveResult =
  PositionAlreadyOccupied |
  KeepPlaying Board |
  FinishedMove FinishedBoard
  deriving (Show, Eq)

whoWon :: FinishedBoard -> GameResult
whoWon (FinishedBoard _ r) = r


-- I define a typeclass because I want my move function to be :
--     - move :: Board -> MoveResult
--     - move :: EmptyBoard -> MoveResult
class Move a where
  move :: Position -> a -> MoveResult


class BoardLike b where
  playerAt :: b -> Position -> Maybe Player

instance BoardLike Board where
  playerAt (Board m) = flip M.lookup m

instance BoardLike FinishedBoard where
  playerAt (FinishedBoard b _) = playerAt b 

instance BoardLike EmptyBoard where
  playerAt _  _ = Nothing


instance Move Board where
   move p b@(Board m) =
     let currPlayer = whoseTurn b
         alreadyOcc = isJust (M.lookup p m)
         allmoves = M.insert p currPlayer m
         moves = playerMoves currPlayer allmoves
         isWin = any (\(a,b,c) -> isJust (mapM (`M.lookup` moves) [a,b,c])) wins
     in
      if alreadyOcc
      then
        PositionAlreadyOccupied
      else
         if isWin
         then
           FinishedMove (FinishedBoard (Board allmoves) (Win currPlayer))
         else
            if M.size allmoves == 9
            then
             FinishedMove (FinishedBoard (Board allmoves) Draw)
            else
             KeepPlaying (Board allmoves)

instance Move MoveResult where
  move p PositionAlreadyOccupied = PositionAlreadyOccupied
  move p (KeepPlaying b) = move p b
  move p (FinishedMove b) = FinishedMove b


instance Move EmptyBoard where
  move p _ = KeepPlaying (Board (M.insert p player1 M.empty))



playerMoves :: Player -> M.Map Position Player -> M.Map Position Player
playerMoves p = M.filter (p ==)

-- | returns the next player
--
-- >>> whoseTurn empty == player1
whoseTurn :: Board -> Player
whoseTurn (Board m) = let nbMoves = M.size . flip playerMoves m
                      in if nbMoves player1 == nbMoves player2 then player1 else player2

wins :: [(Position, Position, Position)]
wins = [(HG,MG,BG),(HM,MM,BM),(HD,MD,BD),
        (HG,HM,HD),(MG,MM,MD),(BG,BM,BD),
        (HG,MM,BD),(MG,MM,MD),(BG,MM,HD)]
