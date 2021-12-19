// 11g0
module Robots

// OBS. det er ikke meningen at noget af det skal virke, det er bare 'signaturen'...
// så hvad vi skal arbejde videre med
// type BoardDisplay =
//     class
//         new : rows:int * cols:int -> BoardDisplay
//         member Set : row:int * col:int * cont:string -> unit
//         member SetBottomWall : row:int * col:int -> unit
//         member SetRightWall : row:int * col:int -> unit
//         member Show : unit -> unit
//     end

// jeg har ændret her bare så jeg kunne få lov til at arbejde videre med det
type BoardDisplay (rows:int, cols:int) =
    member this.Set (row:int, col:int, cont:string) = ()
    member this.SetBottomWall (row:int, col:int) = ()
    member this.SetRightWall (row:int, col:int) = ()
    member this.show () = ()

//11g1
type Position = int*int
type Direction = North | South | East | West
type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore

[<AbstractClass >] 
type BoardElement () =
    abstract member RenderOn : BoardDisplay -> unit
    abstract member Interact : Robot -> Direction -> Action
    default __.Interact _ _ = Ignore
    abstract member GameOver : Robot list -> bool
    default __.GameOver _ = false

and Robot (row:int, col:int, name:string) =
    inherit BoardElement ()
    let mutable pos = (row, col)    // ok
    member this.Position = pos      // ok
    override this.Interact other dir =      // ok
        let (r1, c1), (r2, c2) = other.Position, this.Position
        match dir with
        | North -> if c1+1 = c2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        | South -> if c1-1 = c2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        | East  -> if r1+1 = r2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        | West  -> if r1-1 = r2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        
    override this.RenderOn display =    // Can't test before BoardDisplay is implemented
        display.Set (fst this.Position, snd this.Position, this.Name)

    member val Name = name      // ok

    member robot.Step dir =     // ok
        let (r,c) = robot.Position
        match dir with
        | North -> pos <- (r, c+1)
        | South -> pos <- (r, c-1)
        | East  -> pos <- (r+1, c)
        | West  -> pos <- (r-1, c)

// ok
and Goal (r:int, c:int) =
    inherit BoardElement ()
    override this.RenderOn display = display.Set (r, c, "gg")
    // If any robot in a list robots is on a goal (r, c) the game is over
    override this.GameOver (robots:Robot list) = robots |> List.exists (fun (x:Robot) -> x.Position = (r,c))

// An inner vertical wall
type VerticalWall (r:int, c:int, n:int) =   // Not sure how to test before BoardDisplay is implemented
    inherit BoardElement ()
    override this.RenderOn display = 
        if n > 0 then 
            for i in 0..n do display.Set (r, c+i, "|")
        else 
            for i in 0..n do display.Set (r, c-i, "|")

// An inner horizontal wall
type HorizontalWall (r:int, c:int, n:int) =     // Not sure how to test before BoardDisplay is implemented
    inherit BoardElement ()
    override this.RenderOn display = 
        if n > 0 then 
            for i in 0..n do display.Set (r+i, c, "-")
        else 
            for i in 0..n do display.Set (r-i, c, "-")

(*

type BoardFrame (r:int, c:int) =
    inherit BoardElement ()
*)