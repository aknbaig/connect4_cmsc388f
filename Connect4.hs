main = startConnect4

startConnect4 = do
	putStrLn "Welcome to Connect 4!"
	putStrLn "Would you like to go first (1) or second (2)"
	turnChoice <- getLine
	--putStrLn ("The choice was " ++ turnChoice)
	-- if the user wants to go first the we call runConnect 4 with the playerId set to 0 otherwise we set it to 1
	if turnChoice == "1" then 
		runConnect4 0 [['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-']] 
	else 
		runConnect4 1 [['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-'],
					   ['-','-','-','-','-','-','-']] 

{- This method will run the game and when a winner is picked or a draw occurs, the outcome will be printed to the screen 
This method will run until game state is true which means that a winner has been chosen or a draw has occurred -}
runConnect4 :: Integer -> [[Char]] -> IO ()
runConnect4 playerId board = do
	printGameBoard board 
	let gamestate = finishedGame board in 
		case gamestate of
			(False, _) -> if playerId == 0 then runConnect4 1 (turn 0 board) else runConnect4 0 (turn 1 board)
			(True, 1) -> putStrLn "The computer has won!"
			(True, 0) -> putStrLn "You have won!"
			(True, -1) -> putStrLn "The game has ended in a draw"

{- This will print the game board to the screen -}
printGameBoard :: [[Char]] -> IO ()
printGameBoard board = error "Define me!"

{- This method will either ask the user to make a move or ask the computer to make a move -} 
turn :: Integer -> [[Char]] -> [[Char]]
turn playerId board = if playerId == 0 then playerTurn board else comTurn board

{- Specfic method to handle the players turn, an updated board will be returned -}
playerTurn :: [[Char]] -> [[Char]]
playerTurn board = error "Define me!" 

{- Specfic method to handle the AI/Computer's turn, an updated board will be returned -}
comTurn :: [[Char]] -> [[Char]]
comTurn board = error "Define me!"

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