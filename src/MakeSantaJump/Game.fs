module MakeSantaJump

open System.Collections.Generic 
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input


type SpriteTexture =
    {
        texture : Texture2D;
        textureData : Color array;
        spriteWidth : int;
        numSprites : int;
    }


type Santa(spriteTexture, trigger, startBottom) =
    let santaX = 100
    let santaWidth = 64
    let santaHeight = 72

    let gravity = 0.05f

    let spriteChangeTime = 80.0f

    let mutable y = single(startBottom - santaHeight)
    let mutable dy = 0.0f
    let mutable isJumping = false
    let mutable spriteTimer = 0.0f
    let mutable spriteIndex = 0

    member this.Bounds
        with get() = Rectangle(santaX, int(y), santaWidth, santaHeight)

    member this.Update(deltaTime, isKeyPressedSinceLastFrame : Keys -> bool, trackBounds : Rectangle) =
        // Should we start a jump?
        if not isJumping && isKeyPressedSinceLastFrame(trigger) then
            isJumping <- true
            dy <- -0.7f

        if isJumping then
            // Physics!
            y <- y + dy * deltaTime

            let hasLanded = int(y) + santaHeight >= trackBounds.Bottom
            if hasLanded then
                y <- single(trackBounds.Bottom - santaHeight)
                isJumping <- false
                dy <- 0.0f
            else
                dy <- dy + gravity
        else
            // Update sprite.
            spriteTimer <- spriteTimer + deltaTime
            if spriteTimer >= spriteChangeTime then
                spriteTimer <- 0.0f
                let wrap value max =
                    if value > max then 0 else value
                spriteIndex <- wrap (spriteIndex + 1) (spriteTexture.numSprites - 1)

    member this.Draw(spriteBatch : SpriteBatch) =
        let spriteBounds =
            Rectangle(spriteIndex * spriteTexture.spriteWidth, 0,
                      spriteTexture.spriteWidth, 
                      spriteTexture.texture.Height)
        spriteBatch.Draw(spriteTexture.texture, this.Bounds, 
                         System.Nullable(spriteBounds),
                         Color.White)

    // Used for pixel-perfect collision detection.
    member this.AnyNonTransparentPixels(x1, x2, y1, y2) =
        let xOffset = spriteTexture.spriteWidth * spriteIndex
        let pixelsInsideRegion = seq {
            for y in y1..y2-1 do
                for x in (x1 + xOffset)..(x2 + xOffset - 1) do
                    let index = (y * spriteTexture.texture.Width) + x
                    yield spriteTexture.textureData.[index]
        }
        Seq.exists (fun c -> c <> Color.Transparent) pixelsInsideRegion


type Obstacle(startX, width, height) =
    let speed = -0.3f

    let mutable x = startX

    member this.Visible
        with get() = int(x) + width > 0

    member this.GetBounds(trackBounds : Rectangle) =
        Rectangle(int(x), trackBounds.Bottom - height, width, height)

    member this.Update(deltaTime) =
        x <- x + speed * deltaTime

    member this.Draw(spriteBatch : SpriteBatch, texture, trackBounds : Rectangle) =
        spriteBatch.Draw(texture, this.GetBounds(trackBounds), Color.Green)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Obstacle =
    let rng = System.Random()

    let removeOldObstacles (obstacles : Obstacle list) =
        obstacles |> List.filter (fun o -> o.Visible)

    let addNewObstacles (trackBounds : Rectangle) (obstacles : Obstacle list) =
        let isMostRecentlyAddedObstacleFullyVisible =
            match obstacles with
            | head :: tail -> head.GetBounds(trackBounds).Right < trackBounds.Right
            | [] -> true

        if isMostRecentlyAddedObstacleFullyVisible then
            let x = trackBounds.Right + 200 + rng.Next(200)
            let width = rng.Next(20, 35)
            let height = rng.Next(30, 55)
            let newObstacle = Obstacle(single(x), width, height)
            newObstacle :: obstacles
        else
            obstacles


type Track(color, bounds : Rectangle, spriteTexture, triggerKey) =
    let mutable obstacles = List.empty<Obstacle>
    let santa = Santa(spriteTexture, triggerKey, bounds.Bottom)

    member this.Update(deltaTime, isKeyPressedSinceLastFrame) =
        santa.Update(deltaTime, isKeyPressedSinceLastFrame, bounds)
        obstacles <- obstacles
            |> List.map (fun o -> o.Update(deltaTime); o)
            |> Obstacle.removeOldObstacles
            |> Obstacle.addNewObstacles bounds

    member this.Draw(spriteBatch : SpriteBatch, texture) =
        spriteBatch.Draw(texture, bounds, color) // Track background
        for obstacle in obstacles do
            obstacle.Draw(spriteBatch, texture, bounds)
        santa.Draw(spriteBatch)

    member this.HasCollisions() =
        let santaBounds = santa.Bounds

        let obstacleCollidingWithSanta (obstacle : Obstacle) =
            // First do simple intersection.
            let obstacleBounds = obstacle.GetBounds(bounds)

            if santaBounds.Intersects(obstacleBounds) then
                // If the bounding rectangles overlap, then do pixel-perfect collision detection.
                let x1 = max (obstacleBounds.X - santaBounds.X) 0
                let x2 = min (obstacleBounds.Right - santaBounds.X) santaBounds.Width

                let y1 = max (obstacleBounds.Y - santaBounds.Y) 0
                let y2 = min (obstacleBounds.Bottom - santaBounds.Y) santaBounds.Height

                santa.AnyNonTransparentPixels(x1, x2, y1, y2)
            else
                false

        List.exists obstacleCollidingWithSanta obstacles

 
type MakeSantaJumpGame() as x =
    inherit Game()
 
    do x.Content.RootDirectory <- "Content"
    do x.Window.Title <- "Make Santa Jump"

    let graphics = new GraphicsDeviceManager(x)
    do graphics.PreferredBackBufferWidth <- 800
    do graphics.PreferredBackBufferHeight <- 600

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable texture = Unchecked.defaultof<Texture2D>
    let mutable fontRenderer = Unchecked.defaultof<FontRendering.FontRenderer>

    let mutable tracks = []

    let mutable gameOver = false

    let mutable lastKeyState = KeyboardState()
 
    override x.LoadContent() =
        spriteBatch <- new SpriteBatch(x.GraphicsDevice)

        texture <- new Texture2D(x.GraphicsDevice, 1, 1)
        texture.SetData([| Color.White |])

        use santaStream = System.IO.File.OpenRead("Santa.png")
        let santaTexture = Texture2D.FromStream(x.GraphicsDevice, santaStream)
        let santaTextureData = Array.create<Color> (santaTexture.Width * santaTexture.Height) Color.Transparent
        santaTexture.GetData(santaTextureData)
        let spriteTexture = { texture = santaTexture;
                              textureData = santaTextureData;
                              spriteWidth = santaTexture.Width / 8;
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
            Track(colors.[i], trackBounds, spriteTexture, keys.[i])
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
            for track in tracks do
                track.Update(deltaTime, isKeyPressedSinceLastFrame)

        lastKeyState <- currentKeyState

        if List.exists (fun (t : Track) -> t.HasCollisions()) tracks then
            gameOver <- true

    override x.Draw (gameTime) =
        x.GraphicsDevice.Clear Color.Black

        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)

        for track in tracks do
            track.Draw(spriteBatch, texture)

        fontRenderer.DrawText(spriteBatch, 50, 50, "Hello World!")

        spriteBatch.End()