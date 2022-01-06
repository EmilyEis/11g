// 11g0
module Robots

// jeg har ændret her bare så jeg kunne få lov til at arbejde videre med det
type BoardDisplay (rows:int, cols:int) =
    let positions = Array.allPairs [|1..rows|] [|1..cols|]
    let arrS = [|for n in positions do n, "  "|]
    let arrB = [|for n in positions do n, "  +"|]
    let arrR = [|for n in positions do n, " "|]
    member this.arrSet = arrS
    member this.arrBot = arrB
    member this.arrRig = arrR
    member this.Set (row:int, col:int, cont:string) = 
        try Array.set this.arrSet (this.arrSet |> Array.findIndex (fun elm -> fst elm = (row,col)) ) ((row,col), cont) 
        with _ -> ()
    member this.SetBottomWall (row:int, col:int) = 
        try Array.set this.arrBot (this.arrBot |> Array.findIndex (fun elm -> fst elm = (row,col)) ) ((row,col), "--+") 
        with _ -> ()
    member this.SetRightWall (row:int, col:int) = 
        try Array.set this.arrRig (this.arrRig |> Array.findIndex (fun elm -> fst elm = (row,col)) ) ((row,col), "|") 
        with _ -> ()
    member this.Show () = 
        printf "+"; for i in 0..cols-1 do printf "--+"  // fst line
        for n in 0..rows-1 do
            printf "\n|"
            for x in 0..cols-1 do
                if x = cols-1 then printf "%s|" (try snd (Array.find (fun elm -> fst elm = (n+1, x+1)) this.arrSet) with _ -> "")
                else printf "%s%s" (try snd (Array.find (fun elm -> fst elm = (n+1, x+1)) this.arrSet) with _ -> "") (try snd (Array.find (fun elm -> fst elm = (n+1, x+1)) this.arrRig) with _ -> "")
            printf "\n+"
            for i in 0..cols-1 do 
                if n = rows-1 then printf "--+"  // last line
                else printf "%s" (try snd (Array.find (fun elm -> fst elm = (n+1, i+1)) this.arrBot) with _ -> "")

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
    member this.Position with get () = pos and set (r,c) = pos <- (r,c)     // ok
    override this.Interact other dir =      // ok
        let (r1, c1), (r2, c2) = other.Position, this.Position
        match dir with
        | North -> if c1+1 = c2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        | South -> if c1-1 = c2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        | East  -> if r1+1 = r2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        | West  -> if r1-1 = r2 then Stop (r1, c1) else Continue (dir, (r1, c1))
        
    override this.RenderOn display =    // ok
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

// Sets an inner vertical wall in a board
and VerticalWall (r:int, c:int, n:int) =   // ok
    inherit BoardElement ()
    override this.RenderOn display = 
        for i in 0..n do display.SetRightWall ((if n > 0 then r+i else r-i), c)

// Sets an inner horizontal wall in a board
and HorizontalWall (r:int, c:int, n:int) =     // ok
    inherit BoardElement ()
    override this.RenderOn display = 
        for i in 0..n do display.SetBottomWall (r, if n > 0 then c+i else c-i)

// Not sure what it is supposed to do
and BoardFrame (r:int, c:int) =
    inherit BoardElement ()
    override this.RenderOn display = display.SetBottomWall (r, c); display.SetRightWall (r, c)

type Board (display: BoardDisplay) =
    member this.AddRobot (robot:Robot) = robot.RenderOn display
    member this.AddElement (element:BoardElement) = element.RenderOn display
    member this.Elements = []    // returns BoardElement list
    member this.Robots = []      // returns Robot list
    member this.Move (robot:Robot, dir:Direction) = 
        for x in this.Robots do
            if not (robot = x) then
                match robot.Interact x dir with
                | Continue (dir, (r, c)) -> robot.Step (dir)
                | Stop (r, c)            -> robot.Position <- (r, c)
                | Ignore                 -> robot.Step (dir)


        // while List.exists (fun (x:Robot) -> robot.Interact x dir = Continue (_,_,_)) robots do
        //     List.map (fun (x:Robot) -> robot.Interact x dir) robots