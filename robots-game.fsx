// Game
open Robots

let start = 
    let board = Board (4,7)
    board.AddElement (Goal (3,6))
    board.AddRobot (Robot (2,3, "AA"))
    board.AddRobot (Robot (1,1, "BB"))
    board.AddRobot (Robot (4,7, "CC"))
    board.AddElement (HorizontalWall (1,4,2))
    board.AddElement (VerticalWall (2,3,1))
    board.AddElement (HorizontalWall (2,3,1))
    let game = Game (board)
    game.Play ()
start

