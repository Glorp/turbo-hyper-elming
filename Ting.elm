module Ting where
import Graphics.Input.Field as Field
import Graphics.Input as Input
import String
import Maybe
import Json
import Dict
import Color
import Window

id x = x

type Request = {url : String, verb : String, headers : [String]}
type Response = {respText : String, status : Int, statusText : String, headers : Maybe String}
getReq : String -> Request
getReq s = {url = s, verb = "GET", headers = []}

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


renderStuff j l = let renderD d l = case l of
                                        []           -> renderJson (Json.Object d)
                                        (s, f) :: xs -> (case Dict.get s d of
                                                             Just j  -> above (f j) (renderD (Dict.remove s d) xs)
                                                             Nothing -> renderD d xs)
                  in case j of
                         Json.Object d -> renderD d l
                         _             -> renderJson j

butt h v e = Input.customButton h v (bordered (Color.lightBlue) e)
                                    (bordered (Color.blue) e)
                                    (bordered (Color.darkBlue) e)

renderLink j = case j of
                   Json.Object d -> (case (Dict.get "href" d, Dict.get "rel" d) of
                                         (Just (Json.String h), Just r)  -> beside (renderJson (Json.Object (Dict.remove "rel" (Dict.remove "href" d))))
                                                                                   (butt handle (Just (getReq h)) (renderJson r))
                                         (Just (Json.String h), Nothing) -> beside (renderJson (Json.Object (Dict.remove "href" d)))
                                                                                   (butt handle (Just (getReq h)) (plainText h))
                                         _                               -> renderJson j)
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
                              fieldReqSig = lift (.string >> getReq >> Just) fieldSig
                              reqSig = merge fieldReqSig inp.signal
                          in (field, inp.handle, reqSig)


strToGUI s = case Json.fromString s of
                 Just x -> renderStuff x [("links", renderLinks)]
                 _      -> plainText (String.concat ["not soap? :() ", s])

respToGUI x = case x of
                 Nothing -> plainText "..."
                 Just r  -> flow down [(plainText (String.concat ["Status: " , show (r.status), ", ", r.statusText])),
                                       (case r.headers of
                                            Nothing -> empty
                                            Just h  -> plainText h),
                                       strToGUI r.respText]

port out : Signal (Maybe Request)
port out = reqSig

port inn : Signal (Maybe Response)

centered w e = container w (heightOf e) midTop e

main = let hed  = lift (flow right) (combine [field, constant b])
           stuf = lift (flow right) (combine [lift respToGUI inn])
       in lift (flow down) (combine [lift2 centered Window.width hed,
                                     lift2 centered Window.width stuf])
