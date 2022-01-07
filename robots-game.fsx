// Game
open Robots

let start = 
    let game = Game (Board (4,7))
    game.Play ()

start




//let b1 = BoardDisplay (4,7)
//let bb = b1.arrSet

//b1.Set (1,1, "BB")
//b1.Set (2,3, "AA")
//b1.Set (3,6, "gg")
//b1.Set (4,7, "CC")
//b1.SetBottomWall (1,4)
//b1.SetBottomWall (1,5)
//b1.SetBottomWall (2,3)
//b1.SetRightWall (2,3)

// printfn ""
// let board1 = BoardFrame (4,7)
// let g1 = Goal (3, 6)
// let r1 = Robot (2, 3, "AA")
// let r2 = Robot (1, 1, "BB")
// let r3 = Robot (4, 7, "CC")
// let v1 = VerticalWall (2, 3, 0)
// let h1 = HorizontalWall (2, 3, 0)
// let m1 = Board b1
// m1.AddElement r1
// //board1.RenderOn b1
// //r1.RenderOn b1
// //r2.RenderOn b1
// //r3.RenderOn b1
// m1.AddRobot r1
// m1.AddRobot r2
// m1.AddRobot r3
// m1.AddElement g1
// m1.AddElement v1
// m1.AddElement h1
// // g1.RenderOn b1
// // v1.RenderOn b1
// // h1.RenderOn b1
// //List.iter (fun (x:Robot) -> printfn "pos: %A; name: %s" x.Position x.Name) m1.Robots
// //List.iter (fun (x:BoardElement) -> printfn "%A" x) m1.Elements

// b1.Show ()


// let h = HorizontalWall (2, 3, 2)
// let d = BoardDisplay (5, 5)


//printfn "%A" (h.RenderOn d) // shows nothing right now
// printfn "%A" (r1.Position)              // Position ok
// printfn "%A" (r1.Name)                  // Name ok
// printfn "%A" (r1.Interact r2 South)     // Interact ok
// printfn "%A" (r1.Step West;r1.Position) // Step ok

// let g1 = Goal (2,2)
// printfn "%A" (g1.GameOver [r1;r2])

// let r3 = Robot (0, 0, "C")

// let llst = [r1;r2;r3]

// while g1.GameOver llst = false do 
//     List.map (fun (x:Robot) -> x.Step North; x.Step East; printfn "%A %A" x.Name x.Position) llst

