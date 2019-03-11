main = runConnect4

runConnect4 = do
	putStrLn "Welcome to Connect 4!"
	putStrLn "Would you like to go first (1) or second (2)"
	turnChoice <- getLine
	putStrLn ("The choice was " ++ turnChoice)
	let board  = turn turnChoice []	
	putStrLn "The game is finished"

{- This method will either ask the user to make a move or ask the computer to make a move -} 
turn :: String -> [[Char]] -> [[Char]]
turn turnChoice board = board

 
{- This method will look at a gameboard and determine if the game is over-}
finishedGame :: [[Char]] -> Bool
finishedGame = error "Define me!"