module Graphecs where
import Graphics.Input.Field as Field
import Graphics.Input as Input
import Json
import Dict
import Color

type JsonDict = Dict.Dict String Json.Value
data Elem = Labeled Element | Only Element

spacec : Color.Color -> Element -> Element
spacec c e = color c (container (4 + widthOf e) (4 + heightOf e) middle e)

spacey : Element -> Element
spacey e = spacec Color.white e

bordered : Color.Color -> Element -> Element
bordered c e = spacec c (spacey e)

renderJson : Json.Value -> Element
renderJson x = case x of
                   Json.String s  -> let e = plainText s
                                     in if widthOf e > 500 then width 500 e else e
                   Json.Number n  -> plainText (show n)
                   Json.Boolean b -> plainText (show b)
                   Json.Null      -> plainText "null"
                   Json.Array l   -> flow down (map renderJson l)
                   Json.Object d  -> case renderD d of
                                         [] -> empty
                                         l  -> bordered Color.darkGrey (renderKV l)

renderD : JsonDict -> [(String, Elem)]
renderD d = let render (k, v) = (k, Labeled (renderJson v))
            in map render (Dict.toList d)

renderKV : [(String, Elem)] -> Element
renderKV l = let renderedL = map (\(s, e) -> (plainText (concat [s, ":  "]), e)) l
                 maxOf (le, re) n = case re of
                                        Labeled e -> max (widthOf le) n
                                        _         -> n
                 maxW = foldl maxOf 0 renderedL
                 foo (le, re) = case re of
                                    Labeled e -> beside (leftAl maxW le) e
                                    Only e    -> e
             in flow down (map spacey (map foo renderedL))

renderJsonBut : [(String, Json.Value -> Elem)] -> Json.Value -> Element
renderJsonBut l j =
    let remove d kvs = foldl Dict.remove d (map fst kvs)
        consDKV d (k, f) kvs = case Dict.get k d of
                             Just v  -> (k, f, v) :: kvs
                             Nothing -> kvs
        keyVals d kvs = foldr (consDKV d) [] kvs
        render (k, f, v) = (k, f v)
    in case j of
           Json.Object d -> bordered Color.darkGray (renderKV (concat [map render (keyVals d l), renderD (remove d l)]))
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





