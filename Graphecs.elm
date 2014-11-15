module Graphecs where
import Graphics.Input.Field as Field
import Graphics.Input as Input
import Json
import Dict
import Color

bordered : Color.Color -> Element -> Element
bordered col e = let (x, y) = sizeOf e
                     c      = color Color.white (container (x + 4) (y + 4) middle e)
                 in color col (container (x + 8) (y + 8) middle c)

renderJson : Json.Value -> Element
renderJson x = case x of
                   Json.String s  -> plainText s
                   Json.Number n  -> plainText (show n)
                   Json.Boolean b -> plainText (show b)
                   Json.Null      -> plainText "null"
                   Json.Array l   -> flow right (join [plainText ", "] [map renderJson l])
                   Json.Object d  -> let render (k, v) = flow right [plainText k,
                                                                     plainText ": ",
                                                                     renderJson v]
                                         l = Dict.toList d
                                     in (case l of
                                             [] -> empty
                                             _  -> bordered Color.darkGrey (flow down (map render l)))

renderJsonBut : Json.Value -> [(String, Json.Value -> Element)] -> Element
renderJsonBut j l =
    let renderD d l = case l of
                          []           -> renderJson (Json.Object d)
                          (s, f) :: xs -> (case Dict.get s d of
                                               Just j  -> above (f j) (renderD (Dict.remove s d) xs)
                                               Nothing -> renderD d xs)
    in case j of
           Json.Object d -> renderD d l
           _             -> renderJson j

butt : Input.Handle a -> a -> Element -> Element
butt h v e = Input.customButton h v (bordered (Color.lightBlue) e)
                                    (bordered (Color.blue) e)
                                    (bordered (Color.darkBlue) e)

makeField : String -> String -> (Signal Element, Signal Field.Content)
makeField s ss = let content = Input.input (Field.Content ss (Field.Selection 0 0 Field.Forward))
                     fi = lift (Field.field Field.defaultStyle content.handle identity s) content.signal
                 in (fi, content.signal)

centered : Int -> Element -> Element
centered w e = container w (heightOf e) midTop e
