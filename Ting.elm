module Ting where
import Graphecs as Gfx
import Graphics.Input.Field as Field
import Graphics.Input as Input
import String
import Maybe
import Json
import Dict
import Color
import Window

type Request = {url : String, verb : String, headers : [String]}
type Response = {respText : String, status : Int, statusText : String, headers : Maybe String}

getReq : String -> Request
getReq s = {url = s, verb = "GET", headers = []}

binput = Input.input ()

b = Input.button binput.handle () "beep"

(field, handle, reqSig) = let addr = "meh.txt"
                              inp = Input.input Nothing
                              (field, content) = Gfx.makeField "Thing" addr
                              fieldSig = sampleOn binput.signal content
                              fieldReqSig = lift (.string >> getReq >> Just) fieldSig
                              reqSig = merge fieldReqSig inp.signal
                          in (field, inp.handle, reqSig)

port out : Signal (Maybe Request)
port out = reqSig

port inn : Signal (Maybe Response)

strToGUI : String -> Element
strToGUI s = case Json.fromString s of
                 Just x -> Gfx.renderJsonBut x [("links", renderLinks)]
                 _      -> plainText (String.concat ["not soap? :() ", s])

respToGUI : Maybe Response -> Element
respToGUI x =
    case x of
        Nothing -> plainText "..."
        Just r  -> flow down [(plainText (String.concat ["Status: " , show (r.status), ", ", r.statusText])),
                                       (case r.headers of
                                            Nothing -> empty
                                            Just h  -> plainText h),
                                       strToGUI r.respText]

renderLink : Json.Value -> Element
renderLink j =
    let link s e d = beside (Gfx.renderJson (Json.Object d))
                            (Gfx.butt handle (Just (getReq s)) e)
        rend d = case (Dict.get "href" d, Dict.get "rel" d) of
                     (Just (Json.String h), Just r)  -> link h
                                                             (Gfx.renderJson r)
                                                             (Dict.remove "rel" (Dict.remove "href" d))
                     (Just (Json.String h), Nothing) -> link h
                                                             (plainText h)
                                                             (Dict.remove "href" d)
                     _                               -> Gfx.renderJson j
    in case j of
       Json.Object d -> rend d
       _             -> Gfx.renderJson j

renderLinks : Json.Value -> Element
renderLinks j = case j of
                    Json.Array l -> flow down (map renderLink l)
                    _            -> Gfx.renderJson j


main : Signal Element
main = let hed  = lift (flow right) (combine [field, constant b])
           stuf = lift (flow right) (combine [lift respToGUI inn])
       in lift (flow down) (combine [lift2 Gfx.centered Window.width hed,
                                     lift2 Gfx.centered Window.width stuf])
