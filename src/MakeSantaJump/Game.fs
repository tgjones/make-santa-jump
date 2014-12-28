module MakeSantaJump

open System.Collections.Generic 
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

// Change these values to alter game balance.
let gravity = 0.02f
let speed = -0.2f
let minObstacleWidth = 20
let maxObstacleWidth = 35
let minObstacleHeight = 15
let maxObstacleHeight = 30


type SpriteTexture =
    {
        texture : Texture2D;
        textureData : Color array;
        spriteWidth : int;
        numSprites : int;
    }


type Santa(spriteTexture : SpriteTexture, trigger, startBottom) =
    let santaX = 50
    let santaWidth = spriteTexture.spriteWidth
    let santaHeight = spriteTexture.texture.Height

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
            dy <- -0.45f

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
            let spriteChangeTime = 80.0f
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

    let addNewObstacles (trackBounds : Rectangle) (obstacles : Obstacle list) =
        let isMostRecentlyAddedObstacleFullyVisible =
            match obstacles with
            | head :: tail -> head.GetBounds(trackBounds).Right < trackBounds.Right
            | [] -> true

        if isMostRecentlyAddedObstacleFullyVisible then
            let x = trackBounds.Right + 200 + rng.Next(200)
            let width = rng.Next(minObstacleWidth, maxObstacleWidth)
            let height = rng.Next(minObstacleHeight, maxObstacleHeight)
            let newObstacle = Obstacle(single(x), width, height)
            newObstacle :: obstacles
        else
            obstacles

    let removeOldObstacles (obstacles : Obstacle list) =
        obstacles |> List.filter (fun o -> o.Visible)


type Track(color, bounds : Rectangle, spriteTexture, triggerKey) =
    let mutable obstacles = List.empty<Obstacle>
    let mutable avoidedObstacles = 0
    let santa = Santa(spriteTexture, triggerKey, bounds.Bottom)

    member this.AvoidedObstacles
        with get() = avoidedObstacles

    member this.Update(deltaTime, isKeyPressedSinceLastFrame) =
        santa.Update(deltaTime, isKeyPressedSinceLastFrame, bounds)

        for obstacle in obstacles do
            obstacle.Update(deltaTime)

        let oldObstaclesCount =
            obstacles
            |> List.filter (fun o -> not o.Visible)
            |> List.length
        avoidedObstacles <- avoidedObstacles + oldObstaclesCount

        obstacles <- obstacles
            |> Obstacle.removeOldObstacles
            |> Obstacle.addNewObstacles bounds

    member this.Draw(spriteBatch : SpriteBatch, texture, fontRenderer : FontRendering.FontRenderer) =
        spriteBatch.Draw(texture, bounds, color) // Track background
        for obstacle in obstacles do
            obstacle.Draw(spriteBatch, texture, bounds)
        santa.Draw(spriteBatch)
        fontRenderer.DrawText(spriteBatch, 10, 10 + bounds.Y, triggerKey.ToString())

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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Track =
    let createTracks (gameBounds : Rectangle) spriteTexture numTracks =
        let padding = 10
        let totalPadding = (numTracks - 1) * padding
        let availableHeight = gameBounds.Height - totalPadding
        let trackHeight = availableHeight / numTracks

        let colors = [ Color.Red; Color.Blue; Color.Purple; Color.Brown; Color.Gold ]
        let keys = [ Keys.A; Keys.S; Keys.D; Keys.F; Keys.Space ]

        let makeTrack i =
            let trackBounds = Rectangle(0, i * (trackHeight + padding),
                                        gameBounds.Width, trackHeight)
            Track(colors.[i], trackBounds, spriteTexture,
                  keys.[i + (keys.Length - numTracks)])

        List.init numTracks makeTrack


type GameState =
    | MainMenu
    | Game
    | GamePaused
    | GameOver

type MakeSantaJumpGame() as this =
    inherit Game()
 
    do this.Window.Title <- "Make Santa Jump"

    let graphics = new GraphicsDeviceManager(this)
    do graphics.PreferredBackBufferWidth <- 800
    do graphics.PreferredBackBufferHeight <- 600

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable texture = Unchecked.defaultof<Texture2D>
    let mutable spriteTexture = Unchecked.defaultof<SpriteTexture>
    let mutable fontRenderer = Unchecked.defaultof<FontRendering.FontRenderer>

    let mutable tracks = []
    let mutable gameState = MainMenu
    let mutable lastKeyState = KeyboardState()
 
    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

        texture <- new Texture2D(this.GraphicsDevice, 1, 1)
        texture.SetData([| Color.White |])

        use santaStream = System.IO.File.OpenRead("Santa.png")
        let santaTexture = Texture2D.FromStream(this.GraphicsDevice, santaStream)
        let santaTextureData = Array.create<Color> (santaTexture.Width * santaTexture.Height) Color.Transparent
        santaTexture.GetData(santaTextureData)
        spriteTexture <- { texture = santaTexture;
                           textureData = santaTextureData;
                           spriteWidth = santaTexture.Width / 8;
                           numSprites = 8 }

        use fontTextureStream = System.IO.File.OpenRead("GameFont_0.png")
        let fontTexture = Texture2D.FromStream(this.GraphicsDevice, fontTextureStream)
        let fontFile = FontRendering.FontLoader.Load("GameFont.fnt")
        fontRenderer <- FontRendering.FontRenderer(fontFile, fontTexture)
 
    override this.Update(gameTime) =
        let currentKeyState = Keyboard.GetState()
        let deltaTime = single(gameTime.ElapsedGameTime.TotalMilliseconds)

        let isKeyPressedSinceLastFrame key =
            currentKeyState.IsKeyDown(key) && lastKeyState.IsKeyUp(key)

        match gameState with
        | MainMenu ->
            let startGame numTracks =
                tracks <- Track.createTracks this.GraphicsDevice.Viewport.Bounds spriteTexture numTracks
                gameState <- Game

            if isKeyPressedSinceLastFrame Keys.D1 then startGame 1
            elif isKeyPressedSinceLastFrame Keys.D2 then startGame 2
            elif isKeyPressedSinceLastFrame Keys.D3 then startGame 3
            elif isKeyPressedSinceLastFrame Keys.D4 then startGame 4
            elif isKeyPressedSinceLastFrame Keys.D5 then startGame 5
        | Game ->
            if isKeyPressedSinceLastFrame Keys.P then
                gameState <- GamePaused
            else
                for track in tracks do
                    track.Update(deltaTime, isKeyPressedSinceLastFrame)
                if List.exists (fun (t : Track) -> t.HasCollisions()) tracks then
                    gameState <- GameOver
        | GamePaused ->
            if isKeyPressedSinceLastFrame Keys.P then
                gameState <- Game
        | GameOver ->
            if isKeyPressedSinceLastFrame Keys.Space then
                gameState <- MainMenu

        lastKeyState <- currentKeyState

    override this.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black

        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)

        let avoidedObstacles = List.sumBy (fun (o : Track) -> o.AvoidedObstacles) tracks

        match gameState with
        | MainMenu ->
            fontRenderer.DrawText(spriteBatch, 100, 50, "Choose a difficulty.")
            fontRenderer.DrawText(spriteBatch, 100, 150, "1 - Even Rudolph could handle it")
            fontRenderer.DrawText(spriteBatch, 100, 200, "2 - Not nice")
            fontRenderer.DrawText(spriteBatch, 100, 250, "3 - Ho ho HO NO")
            fontRenderer.DrawText(spriteBatch, 100, 300, "4 - I don't get paid enough for this")
            fontRenderer.DrawText(spriteBatch, 100, 350, "5 - This is why Santa drinks")
        | Game
        | GamePaused ->
            for track in tracks do
                track.Draw(spriteBatch, texture, fontRenderer)
            fontRenderer.DrawText(spriteBatch, this.GraphicsDevice.Viewport.Bounds.Right - 60, 30,
                                  avoidedObstacles.ToString())
        | GameOver ->
            for track in tracks do
                track.Draw(spriteBatch, texture, fontRenderer)
            fontRenderer.DrawText(spriteBatch, this.GraphicsDevice.Viewport.Bounds.Right - 60, 30,
                                  avoidedObstacles.ToString())
            fontRenderer.DrawText(spriteBatch, 100, 100, "Game Over!")
            fontRenderer.DrawText(spriteBatch, 100, 150, "Press Space to continue.")

        spriteBatch.End()