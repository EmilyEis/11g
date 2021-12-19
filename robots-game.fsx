// Game
open Robots

let r1 = Robot (0, 0, "A")
let r2 = Robot (0, 1, "B")

printfn "%A" (r1.Position)              // Position ok
printfn "%A" (r1.Name)                  // Name ok
printfn "%A" (r1.Interact r2 South)     // Interact ok
printfn "%A" (r1.Step West;r1.Position) // Step ok

let g1 = Goal (2,2)
printfn "%A" (g1.GameOver [r1;r2])

let r3 = Robot (0, 0, "C")

let llst = [r1;r2;r3]

while g1.GameOver llst = false do 
    List.map (fun (x:Robot) -> x.Step North; x.Step East; printfn "%A %A" x.Name x.Position) llst

