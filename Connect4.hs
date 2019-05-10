import Data.Char
import System.Random (mkStdGen)
import System.Random (randomRIO)
import System.Random (randomR)
main = startConnect4

startConnect4 = do
	putStrLn "Welcome to Connect 4!"
	putStrLn "Would you like to go first (1) or second (2)"
	turnChoice <- getLine
	--putStrLn ("The choice was " ++ turnChoice)
	let board = [['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-']]
	-- if the user wants to go first the we call runConnect 4 with the playerId set to 0 otherwise we set it to 1
	if turnChoice == "1" then 
		runConnect4 0 board 
	else 
		runConnect4 1 board

{- This method will run the game and when a winner is picked or a draw occurs, the outcome will be printed to the screen 
This method will run until game state is true which means that a winner has been chosen or a draw has occurred -}
runConnect4 :: Integer -> [[Char]] -> IO ()
runConnect4 playerId board = do
	printGameBoard board 
	let gamestate = finishedGame board in 
		case gamestate of
			(False, _) -> if playerId == 0 then do
					putStrLn "Enter column number of row to drop piece (Player): "
					column <- getChar
					runConnect4 1 (dropPiece board ((Data.Char.digitToInt column) - 1) 'P') 
				else do
					-- putStrLn "Enter column number of row to drop piece (Computer): "
					column <- getChar
					let s1 = mkStdGen 42
					col <- randomRIO (0,6)
					putStrLn "Computers has made the move"
					runConnect4 0 (comTurn col board)
					-- runConnect4 0 (dropPiece board ((Data.Char.digitToInt column) - 1) 'C') 
			(True, 1) -> putStrLn "The computer has won!"
			(True, 0) -> putStrLn "You have won!"
			(True, -1) -> putStrLn "The game has ended in a draw"

{- This will print the game board to the screen -}
printGameBoard :: [[Char]] -> IO()
printGameBoard board = 
	case board of
		row:rowt -> do 
			putStrLn row 
			printGameBoard rowt
		[] -> putStr ""	   

{- Specfic method to handle the AI/Computer's turn, an updated board will be returned -}
comTurn :: Int -> [[Char]] -> [[Char]]
comTurn i1 board = let playerToks = (foldBoard board 'P' 6) in
				   let blockCol = (threeInARow board playerToks) in	   
				   if blockCol > (-1) then dropPiece board (fromIntegral blockCol) 'C'
				   else dropPiece board i1 'C'
--(elem (row-3,col) t) && (elem (row-2,col) t) && ((searchBoard board (fromIntegral row - 2) (fromIntegral col)) == '-') then col-1
threeInARow :: [[Char]] -> [(Integer, Integer)] -> Integer
threeInARow board tokPositions = 
	case tokPositions of
		(row,col):t -> (
			let horizontal = (
				if (elem (row,col+1) t) && (elem (row,col+2) t) &&  ((searchBoard board (fromIntegral row-1) (fromIntegral col-2)) == '-') then col-2
				else if (elem (row,col+1) t) && (elem (row,col+2) t) && ((searchBoard board (fromIntegral row-1) (fromIntegral col+2)) == '-') then (col+2)
				else if (elem (row,col+1) t) && ((searchBoard board (fromIntegral row-1) (fromIntegral col+1)) == '-') && (elem (row,col+3) t) then col+1
				else if ((searchBoard board (fromIntegral row-1) (fromIntegral col)) == '-') && (elem (row,col+2) t) && (elem (row,col+3) t) then col
				else -1
				) in
			let vertical = (
				if (elem (row+1,col) t) && (elem (row+2,col) t) && ((searchBoard board (fromIntegral row + 1) (fromIntegral col-1)) == '-') then col-1
				else if (elem (row-1,col) t) && (elem (row+1,col) t) && ((searchBoard board (fromIntegral row + 1) (fromIntegral col-1)) == '-') then col-1
				else if (elem (row-2,col) t) && (elem (row-1,col) t) && ((searchBoard board (fromIntegral row) (fromIntegral col-1)) == '-') then col-1
				-- else if (elem (row+1,col) t) && (elem (row+2,col) t) && ((searchBoard board (fromIntegral row + 2) (fromIntegral col)) == '-') then col-1
				else (-1)
				) in
			let leftDiagonal = (
				if (elem (row+1,col-1) t) && (elem (row+2,col-2) t) && (columnPieces board (fromIntegral col-4) 0) == (fromIntegral row+2) then col-4
				else if (elem (row+1,col-1) t) && (elem (row+2,col-2) t) && (searchBoard board (fromIntegral row-2) (fromIntegral col) == '-') && ((columnPieces board (fromIntegral col) 0) == (fromIntegral row-2)) then col
				else if (elem (row+1,col-1) t) && (elem (row+3,col-3) t) && (searchBoard board (fromIntegral row+1) (fromIntegral col-3) == '-') && ((columnPieces board (fromIntegral col-3) 0) == (fromIntegral row +1)) then col-3
			 	else if (elem (row+2,col-2) t) && (elem (row+3,col-3) t)  && (searchBoard board (fromIntegral row) (fromIntegral col-2) == '-') && ((columnPieces board (fromIntegral col-2) 0) == (fromIntegral row)) then col-2
				else -1
				) in
				let rightDiagonal = (
					if (elem (row+1,col+1) t) && (elem (row+2,col+2) t) && (columnPieces board (fromIntegral col+2) 0) == (fromIntegral row+2) then col+2
					else if (elem (row+1,col+1) t) && (columnPieces board (fromIntegral col+1) 0) == (fromIntegral row+1) && (elem (row+3,col+3) t) && (searchBoard board (fromIntegral row+1) (fromIntegral col+1) == '-')  then col+1
					else if (columnPieces board (fromIntegral col) 0) == (fromIntegral row) && (elem (row+2,col+2) t) && (elem (row+3,col+3) t) && (searchBoard board (fromIntegral row) (fromIntegral col) == '-') then col
					else if (columnPieces board (fromIntegral col-2) 0) == (fromIntegral row-2) && (elem (row+1,col+1) t) && (elem (row+2,col+2) t) && (searchBoard board (fromIntegral row-2) (fromIntegral col-2) == '-') then col-2
					else -1
					) in
			if (horizontal > -1) then horizontal
			else if (vertical > -1) then vertical
			else if (leftDiagonal > -1) then leftDiagonal
			else if (rightDiagonal > -1) then rightDiagonal
			else threeInARow board t
				)
		[] -> -1

searchBoard :: [[Char]] -> Int -> Int -> Char
searchBoard board row col = 
	case board of 
		row1:rowt -> if (row > 0) then (searchBoard rowt (row-1) col) else if (col < 7 && col >= 0) then row1!!(col) else 'z'

columnPieces :: [[Char]] -> Int -> Int -> Int
columnPieces board col acc = 
	case board of 
		row1:rowt -> if (col >=0 && col <=6 && row1!!col /= '-') then columnPieces rowt col (acc+1) else columnPieces rowt col acc 
		[] -> acc

{- drops the piece into the designated column on the game board and returns an updated gameboard -}
dropPiece :: [[Char]] -> Int -> Char -> [[Char]]
dropPiece board column token = 
	case board of
		row1:row2:rowt -> if (row1!!column == '-') && (row2!!column /= '-') then (insertPiece token column row1):row2:rowt else row1:(dropPiece (row2:rowt) column token)
		row:[] -> [(insertPiece token column row)]

{- Inserts a piece into a row of the gameboard -}
insertPiece :: Char -> Int -> [Char] -> [Char]
insertPiece token columnNum row = 
	case row of
		[] -> []
		h:t -> if columnNum == 0 then token:t else h:(insertPiece token (columnNum - 1) t)


{- This method will look at a gameboard and determine if the game is over, 
	if the AI won then the returned integer will be 1 if the player won then 0, if draw then -1 -}
finishedGame :: [[Char]] -> (Bool, Integer)
finishedGame board = 
	let playerToks = (foldBoard board 'P' 6) in
	let	computerToks = (foldBoard board 'C' 6) in
	if (hasWon playerToks) then 
		(True, 0) 
	else if (hasWon computerToks) then 
		(True, 1)
	else if (isBoardFull board) then 
		(True,-1)
	else
		(False,0)

{- This method takes a board row and returns a list of (row,col) positions for a specific token type -}
foldRow :: [Char] -> Char -> Integer -> Integer -> [(Integer, Integer)]
foldRow row token rIndex cIndex = 
	case row of
		tok:tokt -> (
			if (token == tok) then 
				(rIndex,cIndex) : (foldRow tokt token rIndex (cIndex + 1)) 
			else 
				(foldRow tokt token rIndex (cIndex + 1))
				)
		[] -> []

{- This method takes the board and returns a list of ALL (row,col) positions for a specific token type
	Note: rIndex should start as the value of the "top" row of the board (i.e., row 6 in our 6x7 board) -}
foldBoard :: [[Char]] -> Char -> Integer -> [(Integer, Integer)]
foldBoard board token rIndex = 
	case board of
		row:rowt -> (foldBoard rowt token (rIndex - 1)) ++ (foldRow row token rIndex 1)
		[] -> []

{- This method takes the list of all (row,col) positions for a specific token type and returns True if there exists 
	three other token positions that correspond to 4 in a row from the current (row,col) position; either horizontally, 
	vertically, or diagonally (left/right) -}
hasWon :: [(Integer, Integer)] -> Bool
hasWon tokPositions = 
	case tokPositions of
		(row,col):t -> (
			let horizontal = (
				if (elem (row,col+1) t) && (elem (row,col+2) t) && (elem (row,col+3) t) then True
				else if (elem (row,col-1) t) && (elem (row,col+1) t) && (elem (row,col+2) t) then True
				else if (elem (row,col-2) t) && (elem (row,col-1) t) && (elem (row,col+1) t) then True
				else if (elem (row,col-3) t) && (elem (row,col-2) t) && (elem (row,col-1) t) then True
				else False
				) in
			let vertical = (
				if (elem (row+1,col) t) && (elem (row+2,col) t) && (elem (row+3,col) t) then True
				else if (elem (row-1,col) t) && (elem (row+1,col) t) && (elem (row+2,col) t) then True
				else if (elem (row-2,col) t) && (elem (row-1,col) t) && (elem (row+1,col) t) then True
				else if (elem (row-3,col) t) && (elem (row-2,col) t) && (elem (row-1,col) t) then True
				else False
				) in
			let leftDiagonal = (
				if (elem (row+1,col-1) t) && (elem (row+2,col-2) t) && (elem (row+3,col-3) t) then True
				else if (elem (row-1,col+1) t) && (elem (row+1,col-1) t) && (elem (row+2,col-2) t) then True
				else if (elem (row-2,col+2) t) && (elem (row-1,col+1) t) && (elem (row+1,col-1) t) then True
				else if (elem (row-3,col+3) t) && (elem (row-2,col+2) t) && (elem (row-1,col+1) t) then True
				else False
				) in
			let rightDiagonal = (
				if (elem (row+1,col+1) t) && (elem (row+2,col+2) t) && (elem (row+3,col+3) t) then True
				else if (elem (row-1,col-1) t) && (elem (row+1,col+1) t) && (elem (row+2,col+2) t) then True
				else if (elem (row-2,col-2) t) && (elem (row-1,col-1) t) && (elem (row+1,col+1) t) then True
				else if (elem (row-3,col-3) t) && (elem (row-2,col-2) t) && (elem (row-1,col-1) t) then True
				else False
				) in
			if (horizontal || vertical || leftDiagonal || rightDiagonal) then True else hasWon t
				)
		[] -> False

{- Returns True if the board is full, False otherwise -}
isBoardFull :: [[Char]] -> Bool
isBoardFull board = 
	case board of
		row:rowt -> if (elem '-' row) then False else isBoardFull rowt
		[] -> True