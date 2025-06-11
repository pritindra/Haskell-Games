module Chess where
import Data.Char
import Data.List
import Data.Maybe

-- Datas and Types --
type Pos = (Int,Int) -- Pos
type Piece = (Rank, Pos) -- Piece

data Color = Black | White
    deriving (Eq, Show)
data Rank = King | Queen | Knight | Rook | Bishop | Pawn
    deriving (Eq, Show)
data Board = Board ([Piece],[Piece]) -- Board, (whites,blacks)
    deriving (Eq,Show)


-------- BASIC FUNCTIONS ---------
-- takeWhile but includes last elements
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []          =  []
takeWhile' p (x:xs)
    | p x       =  x : takeWhile' p xs
    | otherwise =  [x]
--------------------------------------

-- Create a new game -- 
newBoard :: Board
newBoard = Board (
    [ (Pawn,(i,2)) |i <- [1..8]] ++
    [ (Rook,(1,1)),  (Rook,(8,1)),  (Knight,(7,1))
    ,  (Knight,(2,1)), (Bishop,(3,1)),  (Bishop,(6,1))  -- Whites
    ,  (King,(5,1)) ,  (Queen,(4,1))]
    
    ,[ (Pawn,(i,7)) |i <- [1..8]] ++
    [ (Rook,(1,8)),  (Rook,(8,8)),  (Knight,(7,8))
    ,  (Knight,(2,8)),  (Bishop,(3,8)),  (Bishop,(6,8)) -- Blacks
    ,  (King,(5,8)) ,  (Queen,(4,8))])

-- Board Management --
addPiece :: (Piece,Color) -> Board -> Board
addPiece (p,White) (Board (w,b)) = Board (p:w,b)
addPiece (p,Black) (Board (w,b)) = Board (w,p:b)

getPiece :: Board -> Pos -> Maybe (Piece,Color)
getPiece (Board (white,black)) p = 
                    listToMaybe $ [((r,pos),Black)|(r,pos) <- black, pos == p] 
                     ++ [((r,pos),White)|(r,pos) <- white, pos == p]
                     
removePiece :: Board -> Pos -> Board
removePiece (Board (w,b)) p 
                | getPiece (Board (w,b)) p == Nothing = (Board (w,b))
                | otherwise = Board ([(r,pos)| (r,pos) <- w, pos /= p]
                 ,[(r,pos)| (r,pos) <- b, pos /= p])
         
-- Changes the rank of a piece --         
changePiece :: Board -> Pos -> Rank -> Board
changePiece b p nRank = case getPiece b p of
                Just ((rank, p),c)  -> addPiece ((nRank,p),c) $ removePiece b p  
                _                   -> b   

-- Gets all the opponents on the board                
getOpponents :: Board -> Color -> [Piece]
getOpponents (Board (w,b)) White = b
getOpponents (Board (w,b)) Black = w 

-- Return the king of a color
getKing :: Board -> Color -> Piece
getKing (Board (w,b)) c = case c of
              Black -> getKing' b
              White -> getKing' w
      where
        getKing' :: [Piece] -> Piece
        getKing' ((King,p):ps)  = (King,p) 
        getKing' (p:ps)         = getKing' ps  
        getKing'  _             = (King,(0,0))  
        
-- Movement --  
move :: Board -> Pos -> Pos -> Board
move b p1 p2    | not (elem p2 (validMoves b p1)) || (getPiece b p1)  == Nothing = b   -- Om man försöker flytta till en ogiltig plats -> Board                 -- If move leads to check for enemy
                | (r == Pawn) && ((c == Black && (snd p1)==1) || (c == White && (snd p2)== 8)) = move (changePiece b p1 Queen) p1 p2   -- En pawn ändras till queen
                | otherwise = case getPiece b p2 of
                                 Nothing    -> addPiece ((r,p2),c) (removePiece b p1)    -- Flytta Normal
                                 _          -> addPiece ((r,p2),c) (removePiece (removePiece b p2) p1) --Slå ut en fiendepjäs
    where
        Just ((r,_),c) = getPiece b p1

-- Returns All the valid positions to move to of a Piece on a given position on the board.
validMoves :: Board -> Pos -> [Pos]
validMoves b p | getPiece b p == Nothing = []
               | otherwise = [p' | p' <-(possibleMoves b p)
                , and[(onBoard p'), not(friendly b p p')]]


possibleMoves :: Board -> Pos -> [Pos]
possibleMoves b p = case getPiece b p of
    Just ((Pawn,(x,y)),White)  -> [(x+a,y+1)| a <- [-1..1]
        , ((a==0 && isEmptyPos b (x,y+1)) || (opponent b (x,y) (x+a,y+1) && a /= 0))]
        ++ [(x,y+2)|y == 2]
    Just ((Pawn,(x,y)),Black)  -> [(x+a,y-1)| a <- [-1..1], ((a==0 && isEmptyPos b (x,y-1)) ||
        (opponent b (x,y) (x+a,y-1) && a /= 0))]
        ++ [(x,y-2)|y == 7]
    Just ((Rook,(x,y)),_)      -> clearCrossPath b (x,y)
    Just ((Bishop,(x,y)),_)    -> clearXPath b (x,y)
    Just ((Knight,(x,y)),_)    -> [(x+a,y+b)| a <- [-3,3]
        , b <- [-1,1]] ++ [(x+a,y+b)| a <- [-1,1], b <- [-3,3]]
    Just ((King,(x,y)),_)      -> [(a+x,b+y)|a <- [-1..1], b <- [-1..1]]
    Just ((Queen,(x,y)),_)     -> clearCrossPath b (x,y) ++ clearXPath b (x,y)
    where
        -- Bishops and Queen
        clearXPath b (x,y) = 
               takeWhile'(isEmptyPos b)[(x+i,y+i)|i <- [1..7]]
            ++ takeWhile'(isEmptyPos b)[(x+i,y-i)|i <- [1..7]]
            ++ takeWhile'(isEmptyPos b)[(x-i,y+i)|i <- [1..7]]
            ++ takeWhile'(isEmptyPos b)[(x-i,y-i)|i <- [1..7]]
                
        -- Rooks and Queen
        clearCrossPath b (x,y) = 
               takeWhile'(isEmptyPos b)[(x+i,y)|i <- [1..7]]
            ++ takeWhile'(isEmptyPos b)[(x-i,y)|i <- [1..7]]
            ++ takeWhile'(isEmptyPos b)[(x,y+i)|i <- [1..7]]
            ++ takeWhile'(isEmptyPos b)[(x,y-i)|i <- [1..7]]
        
-- Checks if the pos is empty
isEmptyPos :: Board -> Pos -> Bool
isEmptyPos b p = getPiece b p == Nothing
    
-- Checks if the pos is on the board    
onBoard :: Pos -> Bool
onBoard (x,y) = and [x >= 1, x <= 8, y >= 1, y <= 8]

-- Checks if there is a friendly piece on the pos
friendly :: Board -> Pos -> Pos -> Bool
friendly b p1 p2 | getPiece b p1 == Nothing || getPiece b p2 == Nothing = False
                 | otherwise = c1 == c2
    where 
        Just (_,c1) = getPiece b p1
        Just (_,c2) = getPiece b p2
        
-- Checks if there is an opponent piece on the pos
opponent :: Board -> Pos -> Pos -> Bool
opponent b p1 p2 = not $ or[friendly b p1 p2, isEmptyPos b p2] 

-- Checks if the pos is empty or occupied by an opponent piece
oppOrEmpty :: Board -> Pos -> Pos -> Bool
oppOrEmpty b p1 p2 = (opponent b p1 p2) || (isEmptyPos b p2)

-- Check/CheckMate -- USES validMoves!!!!
check :: Board -> Color -> Bool
check b c = elem kpos oppmoves
    where
        (k,kpos) = getKing b c
        oppmoves = allMoves b (colorInv c)
        
-- All the moves of a single color -- ------ EVIGHETSLOOP ------
allMoves :: Board -> Color -> [Pos]
allMoves b c = concat $ map (valid) $ map (\(r,p) -> p) (getOpponents b (colorInv c))
    where
        valid :: Pos -> [Pos]
        valid p = [p'|p' <- validMoves b p,not(kKing (move b p p') c)]
            where
                kKing :: Board -> Color -> Bool
                kKing b c = elem kPos $ concat $ map (validMoves b) $ map (\(r,p) -> p)$  getOpponents b (c)
                    where
                        (_,kPos) = getKing b c

-- Invers of the Color --
colorInv White = Black
colorInv Black = White
                
-- Is it check mate? --
gameOver :: Board -> Bool
gameOver b = (check b Black && (allMoves b Black) == []) || (check b White && (allMoves b White) == [])

winner :: Board -> Color
winner b | (check b Black && (allMoves b Black) == [])  = White
         | (check b White && (allMoves b White) == [])  = Black

-- Neither black nor white has valid moves
stalemate :: Board -> Bool
stalemate b = not (check b Black || check b White) && (((allMoves b Black) == [])||(allMoves b White) == [])
