// 11g0
module Robots

type BoardDisplay (rows:int, cols:int) =
    let boardArray = Array2D.create rows cols "  "
    let bWalls = Array2D.create (rows+1) cols "  "
    let rWalls = Array2D.create rows (cols+1) " " 

    member this.Set (row:int, col:int, cont:string) =
        boardArray.[row-1,col-1] <- cont

    member this.Get(row:int, col:int) =
        boardArray.[row-1,col-1]

    member this.SetBottomWall (row:int, col:int) = 
        bWalls.[row,col] <- "--"

    member this.SetRightWall (row:int, col:int) =
        rWalls.[row,col] <- "|"

    member this.Show () =
        let mutable toPrint = ""

        // Create the first row frame thing
        toPrint <- toPrint + "+"
        for c in 0..cols-1 do 
            let wall = bWalls.[0,c]
            toPrint <- toPrint + wall + "+"
        toPrint <- toPrint + "\n"

        for r in 0..rows-1 do
            for c in 0..cols-1 do
                let wall = rWalls.[r,c]
                let elem = boardArray.[r,c]
                toPrint <- toPrint + wall + elem
            let w = rWalls.[r,cols]
            toPrint <- toPrint + w + "\n+"
            for c in 0..cols-1 do 
                let wall = bWalls.[r+1,c]
                toPrint <- toPrint + wall + "+"
            toPrint <- toPrint + "\n"
        
        printf "%s" toPrint


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

    let mutable pos = (row, col)          

    member this.Position with get () = pos and set (r,c) = pos <- (r,c)  

    override this.Interact other dir =      
        let (r1, c1), (r2, c2) = other.Position, this.Position
        match dir with
        | North -> if (r1-1, c1) = (r2, c2) then Stop (r1, c1) else Ignore
        | South -> if (r1+1, c1) = (r2, c2) then Stop (r1, c1) else Ignore
        | East  -> if (r1, c1+1) = (r2, c2) then Stop (r1, c1) else Ignore
        | West  -> if (r1, c1-1) = (r2, c2) then Stop (r1, c1) else Ignore

    override this.RenderOn display =    
        display.Set (fst this.Position, snd this.Position, this.Name)

    member val Name = name      

    member this.Step dir =     
        let (r,c) = this.Position
        match dir with
        | North -> this.Position <- (r-1, c)
        | South -> this.Position <- (r+1, c)
        | East  -> this.Position <- (r, c+1)
        | West  -> this.Position <- (r, c-1)
        

// ok
and Goal (r:int, c:int) =
    inherit BoardElement ()

    member this.Position = (r,c)

    override this.RenderOn display = display.Set (r, c, "gg")

    // If any robot in a list robots is on a goal (r, c) the game is over
    override this.GameOver (robots:Robot list) = robots |> List.exists (fun (x:Robot) -> x.Position = (r,c))

// Sets an inner vertical wall in a board
and VerticalWall (r:int, c:int, n:int) =   // ok
    inherit BoardElement ()

    let rows =
        if n<0 then [(r-n)..r-1]
        else [r..(r+n-1)]
    member this.Position = (r,c)

    override this.Interact other dir = 
        let (r1, c1) = other.Position
        if List.contains r1 rows then
            match dir with
                | East -> 
                    if c1 = c then Stop (r1, c1)
                    else Ignore
                | West ->
                    if (c1-1) = c then Stop (r1, c1)
                    else Ignore
                | _ -> Ignore
        else Ignore

        
    override this.RenderOn display = 
        let (row,col) = (r-1, c)
        for i in 0..n-1 do display.SetRightWall(row+i, col)

// Sets an inner horizontal wall in a board
and HorizontalWall (r:int, c:int, n:int) = 
    inherit BoardElement ()

    // List containing the all to columns this wall occupies
    let columns = 
        if n < 0 then [(c-n)..c-1]
        else [c..c+n-1]

    member this.Position = (r,c)

    override this.Interact other dir =
        let (r1, c1)= other.Position
        if List.contains c1 columns then
            match dir with
                | North -> 
                    if (r1-1) = r then Stop (r1, c1)
                    else Ignore
                | South ->
                    if r1 = r then Stop (r1, c1)
                    else Ignore
                | _ -> Ignore
        else Ignore

    override this.RenderOn display = 
        let (row,col) = (r, c-1)
        for i in 0..n-1 do display.SetBottomWall (row, col+i)

// Not sure what it is supposed to do
and BoardFrame (r:int, c:int) =
    inherit BoardElement ()

    member this.Position = (r,c)

    override this.Interact other dir =
        let (r1, c1) = other.Position
        match dir with
        | North -> if r1-1 <= 0 then Stop (1, c1) else Ignore
        | South -> if r1+1 >= r then Stop (r, c1) else Ignore
        | East  -> if c1+1 >= c then Stop (r1, c) else Ignore
        | West  -> if c1-1 <= 0 then Stop (r1, 1) else Ignore

    override this.RenderOn display = 
        for i in 0..r-1 do
            display.SetRightWall(i, 0)
            display.SetRightWall(i, c)
        for i in 0..c-1 do
            display.SetBottomWall(0, i)
            display.SetBottomWall(r, i)

type Board (rows:int, cols:int) =
    let mutable elementList : BoardElement list = []

    let mutable robotList : Robot list = []

    let dis = BoardDisplay (rows, cols)

    member this.size = (rows,cols)

    member this.display = dis

    member this.AddRobot (robot:Robot) =    
        robot.RenderOn this.display
        robotList   <- robot::robotList
        elementList <- robot:>BoardElement::elementList

    member this.AddElement (element:BoardElement) =     
        element.RenderOn this.display
        elementList <- element::elementList

    member this.Elements = elementList

    member this.Robots = robotList

    member this.Move (robot:Robot, dir:Direction) = 
        let rec helper (robot:Robot, dir:Direction) =
            let actions = List.map (fun (x:BoardElement) -> x.Interact robot dir) this.Elements |> List.filter (fun x -> x <> Ignore)
            match actions with
            | [] -> 
                let field = this.display.Get(fst robot.Position, snd robot.Position)
                if not (field = "gg") then
                    this.display.Set (fst robot.Position, snd robot.Position, "  ")
                robot.Step (dir)
                helper (robot, dir)
            | stops ->
                let projection =
                    match dir with
                    | East  | West  -> (fun (Stop s) -> snd s)
                    | South | North -> (fun (Stop s) -> fst s)
                let (Stop (r,c)) = (List.sortBy projection stops).[0]
                this.display.Set (fst robot.Position, snd robot.Position, "  ")
                robot.Position <- (r,c)
                robot.RenderOn this.display

        helper (robot, dir)


type Game (board: Board) =
    let mutable moves = 0
    let mutable isRunning = false

    // member this.Teleport (robot:Robot) = 
    //     let rand = Random()
    //     let (r,c) = (rand.Next (fst board.size), rand.Next (snd board.size))
    //     "j"

    member this.Play () =  // return number of moves before game over
        board.AddElement (BoardFrame board.size)    // game must always have boardframe
        printfn "\nThis is your robots"
        board.Robots |> List.iter (fun (x:Robot) -> printfn "%s: position %A" x.Name x.Position) 
        printfn "\nThis is your board!"
        board.display.Show ()
        isRunning <- true
        while isRunning do
            printfn "Choose a robot to move by writing the name: "
            let name = System.Console.ReadLine ()
            if board.Robots |> List.exists (fun x -> name = x.Name) then
                let robot = board.Robots |> List.find (fun x -> name = x.Name)
                printfn "Use the arrow keys to move the robot!"
                let key = System.Console.ReadKey (true)
                match key.Key with
                | System.ConsoleKey.UpArrow    ->
                    try
                        board.Move (robot, North)
                        moves <- moves + 1
                    with _ -> printfn "Can't go that way!"
                | System.ConsoleKey.DownArrow  -> 
                    try
                        board.Move (robot, South)
                        moves <- moves + 1
                    with _ -> printfn "Can't go that way!"
                | System.ConsoleKey.RightArrow -> 
                    try
                        board.Move (robot, East)
                        moves <- moves + 1
                    with _ -> printfn "Can't go that way!"
                | System.ConsoleKey.LeftArrow  -> 
                    try
                        board.Move (robot, West)
                        moves <- moves + 1
                    with _ -> printfn "Can't go that way!"
                | _ -> printfn "Not correct. Please try again."
                board.display.Show ()
            else printfn "No such robot."
            if board.Elements |> List.exists (fun (x:BoardElement) -> x.GameOver (board.Robots) = true) then
                printfn "You won in %i moves!" moves
                isRunning <- false
        moves
            

