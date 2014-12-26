open MakeSantaJump

[<EntryPoint>]
let main argv = 
    use g = new MakeSantaJumpGame()
    g.Run()
    0