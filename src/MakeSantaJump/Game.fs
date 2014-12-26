module MakeSantaJump

open System.Collections.Generic 
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input


module SantaClaus =
    type Santa =
        {
            trigger : Keys;
            y : single; 
            dy : single; 
            isJumping : bool;
            timer : single;
            spriteIndex : int
        }

    type SantaTexture =
        {
            texture : Texture2D;
            textureData : Color array;
            spriteWidth : int;
            numSprites : int;
        }

    let santaX = 100
    let santaWidth = 64
    let santaHeight = 72
    let gravity = 0.05f

    let spriteChangeTime = 80.0f

    let getSantaBounds (santa : Santa) =
        Rectangle(santaX, int(santa.y), santaWidth, santaHeight)

    let updateSanta deltaTime (isKeyPressedSinceLastFrame : Keys -> bool) (trackBounds : Rectangle) (texture : SantaTexture) santa =
        let santa' =
            if not santa.isJumping && isKeyPressedSinceLastFrame(santa.trigger) then
                { santa with isJumping = true; dy = -0.7f }
            else
                santa

        if santa'.isJumping then
            // Do "physics"
            let santa'' = { santa' with y = santa'.y + santa'.dy * deltaTime}
            if int(santa''.y) + santaHeight > trackBounds.Bottom then
                { santa'' with y = single(trackBounds.Bottom - santaHeight);
                               isJumping = false; dy = 0.0f }
            else
                { santa'' with dy = santa''.dy + gravity }
                
        else
            // Update sprite.
            let timer = santa'.timer + deltaTime
            if timer >= spriteChangeTime then
                let newSpriteIndex = santa'.spriteIndex + 1
                let newSpriteIndex' = if newSpriteIndex >= texture.numSprites then 0 else newSpriteIndex
                { santa' with timer = 0.0f; spriteIndex = newSpriteIndex' }
            else
                { santa' with timer = timer }

    let getSantaSpriteBounds (texture : SantaTexture) i =
        Rectangle(i * texture.spriteWidth, 0, texture.spriteWidth, texture.texture.Height)

    let drawSanta (spriteBatch : SpriteBatch) (texture : SantaTexture) (santa : Santa) =
        let santaSpriteBounds = getSantaSpriteBounds texture santa.spriteIndex
        spriteBatch.Draw(texture.texture, getSantaBounds santa, System.Nullable(santaSpriteBounds), Color.White)


module Obstacles =
    type Obstacle =
        {
            x : single;
            width: int;
            height: int
        }

    let speed = -0.3f

    let getObstacleBounds (trackBounds : Rectangle) (obstacle : Obstacle) =
        Rectangle(int(obstacle.x), trackBounds.Bottom - obstacle.height, obstacle.width, obstacle.height)

    let updateObstacle deltaTime obstacle =
        { obstacle with x = obstacle.x + speed * deltaTime }

    let drawObstacle (spriteBatch : SpriteBatch) texture (trackBounds : Rectangle) (obstacle : Obstacle) =
        spriteBatch.Draw(texture, getObstacleBounds trackBounds obstacle, Color.Green)

    let removeOldObstacles (obstacles : Obstacle list) =
        let isStillVisible obstacle = int(obstacle.x) + obstacle.width > 0
        obstacles |> List.filter isStillVisible

    let rng = System.Random()

    let addNewObstacles (trackBounds : Rectangle) (obstacles : Obstacle list) =
        let isMostRecentlyAddedObstacleFullyVisible =
            match obstacles with
            | head :: tail -> int(head.x) + head.width < trackBounds.Right
            | [] -> true

        if isMostRecentlyAddedObstacleFullyVisible then
            let x = trackBounds.Right + 200 + rng.Next(200)
            let width = rng.Next(20, 35)
            let height = rng.Next(30, 55)
            let newObstacle = { x = single(x);
                                width = width;
                                height = height }
            newObstacle :: obstacles
        else
            obstacles


module Tracks =
    type Track =
        {
            color : Color;
            bounds : Rectangle;
            santa : SantaClaus.Santa;
            obstacles : Obstacles.Obstacle list
        }

    let makeTrack color (bounds : Rectangle) triggerKey =
        {
            color = color;
            bounds = bounds;
            santa = { trigger = triggerKey;
                      y = single(bounds.Bottom - SantaClaus.santaHeight);
                      isJumping = false;
                      dy = 0.0f;
                      timer = 0.0f;
                      spriteIndex = 0 }
            obstacles = []
        }

    let updateTrack deltaTime isKeyPressedSinceLastFrame santaTexture (track : Track) =
        {
            track with
                santa = SantaClaus.updateSanta deltaTime isKeyPressedSinceLastFrame track.bounds santaTexture track.santa;
                obstacles = track.obstacles
                    |> List.map (Obstacles.updateObstacle deltaTime)
                    |> Obstacles.removeOldObstacles
                    |> Obstacles.addNewObstacles track.bounds
        }

    let checkForCollisions (santaTexture : SantaClaus.SantaTexture) (track : Track) =
        let santaBounds = (SantaClaus.getSantaBounds track.santa)
        let obstacleCollidingWithSanta obstacle =
            // First do simple intersection.
            let obstacleBounds = Obstacles.getObstacleBounds track.bounds obstacle
            if santaBounds.Intersects(obstacleBounds) then
                // Then do pixel-perfect collision detection.
                
                let x1 = max (obstacleBounds.X - santaBounds.X) 0
                let x2 = min (obstacleBounds.Right - santaBounds.X) santaBounds.Width

                let y1 = max (obstacleBounds.Y - santaBounds.Y) 0
                let y2 = min (obstacleBounds.Bottom - santaBounds.Y) santaBounds.Height

                let spriteIndex = track.santa.spriteIndex
                let xOffset = santaTexture.spriteWidth * spriteIndex

                let pixelsInsideObstacle = seq {
                    for y in y1..y2-1 do
                        for x in (x1 + xOffset)..(x2 + xOffset - 1) do
                            let index = (y * santaTexture.texture.Width) + x
                            yield santaTexture.textureData.[index]
                }
                Seq.exists (fun c -> c <> Color.Transparent) pixelsInsideObstacle
            else
                false

        List.exists obstacleCollidingWithSanta track.obstacles

    let drawTrack (spriteBatch : SpriteBatch) texture santaTexture (track : Track) =
        spriteBatch.Draw(texture, track.bounds, track.color)
        track.obstacles |> List.iter (Obstacles.drawObstacle spriteBatch texture track.bounds)
        SantaClaus.drawSanta spriteBatch santaTexture track.santa

 
type MakeSantaJumpGame() as x =
    inherit Game()
 
    do x.Content.RootDirectory <- "Content"
    do x.Window.Title <- "Make Santa Jump"

    let graphics = new GraphicsDeviceManager(x)
    do graphics.PreferredBackBufferWidth <- 800
    do graphics.PreferredBackBufferHeight <- 600

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable texture = Unchecked.defaultof<Texture2D>
    let mutable santaTexture = Unchecked.defaultof<SantaClaus.SantaTexture>
    let mutable fontRenderer = Unchecked.defaultof<FontRendering.FontRenderer>

    let mutable tracks = []

    let mutable gameOver = false

    let mutable lastKeyState = KeyboardState()
 
    override x.LoadContent() =
        spriteBatch <- new SpriteBatch(x.GraphicsDevice)

        texture <- new Texture2D(x.GraphicsDevice, 1, 1)
        texture.SetData([| Color.White |])

        use santaStream = System.IO.File.OpenRead("Santa.png")
        let santaTexture' = Texture2D.FromStream(x.GraphicsDevice, santaStream)
        let santaTextureData = Array.create<Color> (santaTexture'.Width * santaTexture'.Height) Color.Transparent
        santaTexture'.GetData(santaTextureData)
        santaTexture <- { texture = santaTexture';
                          textureData = santaTextureData;
                          spriteWidth = santaTexture'.Width / 8;
                          numSprites = 8 }

        let numTracks = 2
        let padding = 10
        let totalPadding = (numTracks - 1) * padding
        let gameBounds = x.GraphicsDevice.Viewport.Bounds
        let availableHeight = gameBounds.Height - totalPadding
        let trackHeight = availableHeight / numTracks

        let colors = [ Color.Red; Color.Blue; Color.Purple; Color.Brown; Color.Gold ]
        let keys = [ Keys.A; Keys.S; Keys.D; Keys.F; Keys.Space ]

        let makeTrack' i =
            let trackBounds = Rectangle(0, i * (trackHeight + padding), gameBounds.Width, trackHeight)
            Tracks.makeTrack colors.[i] trackBounds keys.[i]

        tracks <- List.init numTracks makeTrack'

        use fontTextureStream = System.IO.File.OpenRead("GameFont_0.png")
        let fontTexture = Texture2D.FromStream(x.GraphicsDevice, fontTextureStream)
        let fontFile = FontRendering.FontLoader.Load("GameFont.fnt")
        fontRenderer <- FontRendering.FontRenderer(fontFile, fontTexture)

        ()
 
    override x.Update (gameTime) =
        let currentKeyState = Keyboard.GetState()
        let deltaTime = single(gameTime.ElapsedGameTime.TotalMilliseconds)

        let isKeyPressedSinceLastFrame key =
            currentKeyState.IsKeyDown(key) && lastKeyState.IsKeyUp(key)

        if not gameOver then
            tracks <- tracks |> List.map (Tracks.updateTrack deltaTime isKeyPressedSinceLastFrame santaTexture)

        lastKeyState <- currentKeyState

        if List.exists (Tracks.checkForCollisions santaTexture) tracks then
            gameOver <- true

    override x.Draw (gameTime) =
        x.GraphicsDevice.Clear Color.Black

        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)

        List.iter (Tracks.drawTrack spriteBatch texture santaTexture) tracks

        fontRenderer.DrawText(spriteBatch, 50, 50, "Hello World!")

        spriteBatch.End()