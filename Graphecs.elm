module Graphecs where
import Graphics.Input.Field as Field
import Graphics.Input as Input
import Json
import Dict
import Color

spacec : Color.Color -> Element -> Element
spacec c e = color c (container (4 + widthOf e) (4 + heightOf e) middle e)

spacey : Element -> Element
spacey e = spacec Color.white e

bordered : Color.Color -> Element -> Element
bordered c e = spacec c (spacey e)

renderJson : Json.Value -> Element
renderJson x = case x of
                   Json.String s  -> plainText s
                   Json.Number n  -> plainText (show n)
                   Json.Boolean b -> plainText (show b)
                   Json.Null      -> plainText "null"
                   Json.Array l   -> flow right (join [plainText ", "] [map renderJson l])
                   Json.Object d  -> case renderD d of
                                         [] -> empty
                                         l  -> bordered Color.darkGrey (renderKV l)

renderD : Dict.Dict String Json.Value -> [(Element, Element)]
renderD d = let render (k, v) = (plainText (concat [k, ": "]), renderJson v)
            in map render (Dict.toList d)

renderKV : [(Element, Element)] -> Element
renderKV l = let maxW = foldl (\(e, _) n -> max (widthOf e) n) 0 l
                 foo (e1, e2) = beside (leftAl maxW e1) e2
             in flow down (map spacey (map foo l))

renderJsonBut : Json.Value -> [(String, Json.Value -> Element)] -> Element
renderJsonBut j l =
    let remove d kvs = foldl Dict.remove d (map fst kvs)
        consDKV d (k, f) kvs = case Dict.get k d of
                             Just v  -> (k, f, v) :: kvs
                             Nothing -> kvs
        keyVals d kvs = foldr (consDKV d) [] kvs
        render (k, f, v) = (plainText (concat [k, ": "]), f v)
    in case j of
           Json.Object d -> renderKV (concat [map render (keyVals d l), renderD (remove d l)])
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

leftAl : Int -> Element -> Element
leftAl w e = container w (heightOf e) topLeft e





