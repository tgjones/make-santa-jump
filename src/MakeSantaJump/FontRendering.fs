module FontRendering

// Converted to F# from the original C#.

// ---- AngelCode BmFont XML serializer ----------------------
// ---- By DeadlyDan @ deadlydan@gmail.com -------------------
// ---- There's no license restrictions, use as you will. ----
// ---- Credits to http://www.angelcode.com/ -----------------

open System
open System.Collections.Generic
open System.IO
open System.Xml.Serialization
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<Serializable>]
type FontInfo() =
    let mutable _padding = Rectangle()
    let mutable _spacing = Point()

    [<XmlAttribute("face")>]
    member val Face = "" with get, set

    [<XmlAttribute("size")>]
    member val Size = 0 with get, set

    [<XmlAttribute("bold")>]
    member val Bold = 0 with get, set

    [<XmlAttribute("italic")>]
    member val Italic = 0 with get, set

    [<XmlAttribute("charset")>]
    member val CharSet = "" with get, set

    [<XmlAttribute("unicode")>]
    member val Unicode = 0 with get, set

    [<XmlAttribute("stretchH")>]
    member val StretchHeight = 0 with get, set

    [<XmlAttribute("smooth")>]
    member val Smooth = 0 with get, set

    [<XmlAttribute("aa")>]
    member val SuperSampling = 0 with get, set

    [<XmlAttribute("padding")>]
    member x.Padding
        with get() = sprintf "%d,%d,%d,%d" _padding.X _padding.Y _padding.Width _padding.Height
        and set(value : string) = 
            let padding = value.Split(',')
            _padding <- Rectangle(Convert.ToInt32(padding.[0]),
                                  Convert.ToInt32(padding.[1]),
                                  Convert.ToInt32(padding.[2]),
                                  Convert.ToInt32(padding.[3]))

    [<XmlAttribute("spacing")>]
    member x.Spacing
        with get() = sprintf "%d,%d" _spacing.X _spacing.Y
        and set(value : string) = 
            let spacing = value.Split(',')
            _spacing <- Point(Convert.ToInt32(spacing.[0]),
                              Convert.ToInt32(spacing.[1]))

    [<XmlAttribute("outline")>]
    member val OutLine = 0 with get, set

[<Serializable>]
type FontCommon() =
    [<XmlAttribute("lineHeight")>]
    member val LineHeight = 0 with get, set

    [<XmlAttribute("base")>]
    member val Base = 0 with get, set

    [<XmlAttribute("scaleW")>]
    member val ScaleW = 0 with get, set

    [<XmlAttribute("scaleH")>]
    member val ScaleH = 0 with get, set

    [<XmlAttribute("pages")>]
    member val Pages = 0 with get, set

    [<XmlAttribute("packed")>]
    member val Packed = 0 with get, set

    [<XmlAttribute("alphaChnl")>]
    member val AlphaChannel = 0 with get, set

    [<XmlAttribute("redChnl")>]
    member val RedChannel = 0 with get, set

    [<XmlAttribute("greenChnl")>]
    member val GreenChannel = 0 with get, set

    [<XmlAttribute("blueChnl")>]
    member val BlueChannel = 0 with get, set

[<Serializable>]
type FontPage() =
    [<XmlAttribute("id")>]
    member val ID = 0 with get, set

    [<XmlAttribute("file")>]
    member val File = "" with get, set

[<Serializable>]
type FontChar() =
    [<XmlAttribute("id")>]
    member val ID = 0 with get, set

    [<XmlAttribute("x")>]
    member val X = 0 with get, set

    [<XmlAttribute("y")>]
    member val Y = 0 with get, set

    [<XmlAttribute("width")>]
    member val Width = 0 with get, set

    [<XmlAttribute("height")>]
    member val Height = 0 with get, set

    [<XmlAttribute("xoffset")>]
    member val XOffset = 0 with get, set

    [<XmlAttribute("yoffset")>]
    member val YOffset = 0 with get, set

    [<XmlAttribute("xadvance")>]
    member val XAdvance = 0 with get, set

    [<XmlAttribute("page")>]
    member val Page = 0 with get, set

    [<XmlAttribute("chnl")>]
    member val Channel = 0 with get, set

[<Serializable>]
type FontKerning() =
    [<XmlAttribute("first")>]
    member val First = 0 with get, set

    [<XmlAttribute("second")>]
    member val Second = 0 with get, set

    [<XmlAttribute("amount")>]
    member val Amount = 0 with get, set

[<Serializable>]
[<XmlRoot("font")>]
type FontFile() =
    [<XmlElement("info")>]
    member val Info = Unchecked.defaultof<FontInfo> with get, set

    [<XmlElement("common")>]
    member val Common = Unchecked.defaultof<FontCommon> with get, set

    [<XmlArray("pages")>]
    [<XmlArrayItem("page")>]
    member val Pages = Unchecked.defaultof<List<FontPage>> with get, set

    [<XmlArray("chars")>]
    [<XmlArrayItem("char")>]
    member val Chars = Unchecked.defaultof<List<FontChar>> with get, set

    [<XmlArray("kernings")>]
    [<XmlArrayItem("kerning")>]
    member val Kernings = Unchecked.defaultof<List<FontKerning>> with get, set

module FontLoader =
    let Load(filename : string) =
        let deserializer = new XmlSerializer(typeof<FontFile>)
        let textReader = new StreamReader(filename)
        let file = deserializer.Deserialize(textReader) :?> FontFile
        textReader.Close()
        file

type FontRenderer(fontFile : FontFile, fontTexture : Texture2D) =
    let createCharacterMap() =
        let result = new Dictionary<char, FontChar>()
        for fontCharacter in fontFile.Chars do
            let c = (char) fontCharacter.ID
            result.Add(c, fontCharacter)
        result

    let characterMap = createCharacterMap()

    member this.DrawText(spriteBatch : SpriteBatch, x : int, y : int, text) =
        let mutable dx = x
        let mutable dy = y

        for c in text do
            match characterMap.TryGetValue(c) with
            | (true, fc) ->
                let sourceRectangle = Rectangle(fc.X, fc.Y, fc.Width, fc.Height)
                let position = Vector2(single(dx + fc.XOffset), single(dy + fc.YOffset))

                spriteBatch.Draw(fontTexture, position, Nullable(sourceRectangle), Color.White)
                dx <- dx + fc.XAdvance
            | (false, _) -> ()