// 11g0
module Robots

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
    member this.Step dir =     // ok
        let (r,c) = this.Position
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
    override this.Interact other dir = 
        let (r1, c1) = other.Position
        let llst = [for i in 0..n do (r, c+i)]
        match dir with 
        | North -> if List.contains (r1, c1+1) llst then Stop (r1, c1+1) else Continue (dir, (r1, c1))        
        | South -> if List.contains (r1, c1-1) llst then Stop (r1, c1-1) else Continue (dir, (r1, c1))
        | _ -> Ignore
    override this.RenderOn display = 
        for i in 0..n do display.SetRightWall ((if n > 0 then r+i else r-i), c)

// Sets an inner horizontal wall in a board
and HorizontalWall (r:int, c:int, n:int) = 
    inherit BoardElement ()
    override this.Interact other dir =
        let (r1, c1)= other.Position
        let llst = [for i in 0..n do (r+i, c)]
        match dir with
        | East -> if List.contains (r1+1, c1) llst then Stop (r1+1, c1) else Continue (dir, (r1, c1))
        | West -> if List.contains (r1-1, c1) llst then Stop (r1-1, c1) else Continue (dir, (r1, c1))
        | _ -> Ignore
    override this.RenderOn display = 
        for i in 0..n do display.SetBottomWall (r, if n > 0 then c+i else c-i)

// Not sure what it is supposed to do
and BoardFrame (r:int, c:int) =
    inherit BoardElement ()
    override this.Interact other dir =
        let (r1, c1) = other.Position
        let llst = List.allPairs [1..r] [1..c]
        match dir with
        | North -> if List.contains (r1, c1+1) llst then Stop (r1, c1+1) else Continue (dir, (r1, c1))        
        | South -> if List.contains (r1, c1-1) llst then Stop (r1, c1-1) else Continue (dir, (r1, c1))
        | East  -> if List.contains (r1+1, c1) llst then Stop (r1+1, c1) else Continue (dir, (r1, c1))
        | West  -> if List.contains (r1-1, c1) llst then Stop (r1-1, c1) else Continue (dir, (r1, c1))
    override this.RenderOn display = 
        for i in 0..c-1 do display.Set (r, i, "--+")
        for n in 0..r-1 do display.Set (n, c, "|")
        //display.SetBottomWall (r, c); display.SetRightWall (r, c)  // ???

type Board (rows:int, cols:int) =
    let mutable elementList = []
    let mutable robotList = []
    let dis = BoardDisplay (rows, cols)
    member this.display = dis
    member this.AddRobot (robot:Robot) =    
        robot.RenderOn this.display
        robotList <- robot::robotList
        //elementList <- robot::elementList
    member this.AddElement (element:BoardElement) =     
        element.RenderOn this.display
        elementList <- element::elementList
    member this.Elements = elementList
    member this.Robots = robotList
    member this.Move (robot:Robot, dir:Direction) = 
        for x in this.Robots do
            if not (robot = x) then
                match robot.Interact x dir with
                | Continue (dir, (r, c)) -> robot.Step (dir)
                | Stop (r, c)            -> robot.Position <- (r, c); robot.RenderOn this.display
                | Ignore                 -> robot.Step (dir)


type Game (board: Board) =
    let mutable moves = 0
    member this.Play () =  // return number of moves before game over
        System.Console.Clear ()
        printfn "------------ Let's play Ricochet Robots! ------------\n"
        board.AddElement (BoardFrame (4,7))
        let AA = Robot (1,1,"AA")
        board.AddRobot AA
        board.Move (AA, East)
        printfn "\nThis is your board!"
        board.display.Show ()
        
        
