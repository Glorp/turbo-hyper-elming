module Ting where
import Graphics.Input.Field as Field
import Graphics.Input as Input
import String
import Maybe
import Http
import Json
import Dict
import Color

id x = x

bordered e = let (x, y) = sizeOf e
                 c      = color Color.white (container (x + 4) (y + 4) middle e)
             in color Color.darkGrey (container (x + 8) (y + 8) middle c)

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
                                     in bordered (flow down (map render (Dict.toList d)))


renderStuff j l = let renderD d l = case l of
                                        []           -> renderJson (Json.Object d)
                                        (s, f) :: xs -> (case Dict.get s d of
                                                             Just j  -> above (f j) (renderD (Dict.remove s d) xs)
                                                             Nothing -> renderD d xs)
                  in case j of
                         Json.Object d -> renderD d l
                         _             -> renderJson j

renderLink j = case j of
                   Json.Object d -> (case (Dict.get "href" d) of
                                         Just (Json.String s)  -> beside (renderJson (Json.Object d))
                                                                         (Input.button handle (Just (Http.get s)) "boop")
                                         _                     -> renderJson j)
                   _             -> renderJson j

renderLinks j = case j of
                    Json.Array l -> flow down (map renderLink l)
                    _            -> renderJson j



makeField s ss = let content = Input.input (Field.Content ss (Field.Selection 0 0 Field.Forward))
                     fi = lift (Field.field Field.defaultStyle content.handle id s) content.signal
                 in (fi, content.signal)

binput = Input.input ()

b = Input.button binput.handle () "beep"

(field, handle, reqSig) = let addr = "meh.txt"
                              inp = Input.input Nothing
                              (field, content) = makeField "Thing" addr
                              fieldSig = sampleOn binput.signal content
                              fieldReqSig = lift (.string >> Http.get >> Just) fieldSig
                              mergef mReq req = case mReq of
                                                    Just r  -> r
                                                    Nothing -> req
                              reqSig = foldp mergef (Http.get addr) (merge fieldReqSig inp.signal)
                          in (field, inp.handle, reqSig)



res = Http.send reqSig

respToStr x = case x of
                 Http.Success s   -> s
                 Http.Waiting     -> ":|"
                 Http.Failure i s -> String.append ":((( " s

strToGUI s = case Json.fromString s of
                 Just x -> renderStuff x [("links", renderLinks)]
                 _      -> plainText (String.concat ["not soap? :() ", s])

port out : Signal String
port out = lift respToStr res

port inn : Signal Json.Value

main = lift (flow down) (combine [lift (flow right) (combine [field, constant b]),
                                  lift (flow right) (combine [lift (respToStr >> strToGUI) res])])
