main = startConnect4

startConnect4 = do
	putStrLn "Welcome to Connect 4!"
	putStrLn "Would you like to go first (1) or second (2)"
	turnChoice <- getLine
	--putStrLn ("The choice was " ++ turnChoice)
	-- if the user wants to go first the we call runConnect 4 with the playerId set to 0 otherwise we set it to 1
	if turnChoice == "1" then runConnect4 0 [] else runConnect4 1 []

{- This method will run the game and when a winner is picked or a draw occurs, the outcome will be printed to the screen 
This method will run until game state is true which means that a winner has been chosen or a draw has occurred 
-}
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

{- Specfic method to handle the players turn, an updated board will be returned-}
playerTurn board = error "Define me!" 

{- Specfic method to handle the AI/Computer's turn, an updated board will be returned-}
comTurn board = error "Define me!"

{- This method will look at a gameboard and determine if the game is over, if the AI won then the returned integer will be 1 if the player won then 0, if draw then -1-}
finishedGame :: [[Char]] -> (Bool, Integer)
finishedGame = error "Define me!"