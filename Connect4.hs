main = startConnect4

startConnect4 = do
	putStrLn "Welcome to Connect 4!"
	putStrLn "Would you like to go first (1) or second (2)"
	turnChoice <- getLine
	putStrLn ("The choice was " ++ turnChoice)
	runConnect4 turnChoice False [] 

{- This method will run the game and when a winner is picked or a draw occurs, the outcome will be printed to the screen 
This method will run until game state is true which means that a winner has been chosen or a draw has occurred 
-}
runConnect4 :: String -> Bool -> [[Char]] -> IO ()
runConnect4 turnChoice gameState board = putStrLn "Write the run game method!"

{- This method will either ask the user to make a move or ask the computer to make a move -} 
turn :: String -> [[Char]] -> [[Char]]
turn turnChoice board = board

{- This method will look at a gameboard and determine if the game is over-}
finishedGame :: [[Char]] -> Bool
finishedGame = error "Define me!"