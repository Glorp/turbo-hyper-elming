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

strToGUI : String -> Dict.Dict String (Dict.Dict String Field.Content) -> Element
strToGUI s fs =
    let renderl = [("properties", Gfx.renderJsonBut [("name", Gfx.renderJson),
                                                     ("description", Gfx.renderJson)]),
                   ("links", renderLinks),
                   ("actions", renderActions fs)]
    in case Json.fromString s of
           Just x -> Gfx.renderJsonBut renderl x
           _      -> plainText (concat ["not soap? :() ", s])

respToGUI : Maybe Response -> Dict.Dict String (Dict.Dict String Field.Content) -> Element
respToGUI x fs =
    case x of
        Nothing -> plainText "..."
        Just r  -> flow down [(plainText (concat ["Status: " , show (r.status), ", ", r.statusText])),
                                         (case r.headers of
                                              Nothing -> empty
                                              Just h  -> plainText h),
                                         strToGUI r.respText fs]

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

actionFieldInp = Input.input Nothing

actionFieldSig = let foo x d = case x of
                                   Nothing        -> Dict.empty
                                   Just (a, f, c) -> insContent a f c d
                 in foldp foo Dict.empty (merge actionFieldInp.signal (lift (\_ -> Nothing) reqSig))

getContent a f d =
    let defaultC = Field.Content "" (Field.Selection 0 0 Field.Forward)
    in case Dict.get a d of
           Just x -> (case Dict.get f x of
                          Just c -> c
                          _      -> defaultC)
           _      -> defaultC

insContent a f c d = case Dict.get a d of
                          Just dd -> Dict.insert a (Dict.insert f c dd) d
                          _       -> Dict.insert a (Dict.singleton f c) d

renderAction : Dict.Dict String (Dict.Dict String Field.Content) -> Json.Value -> Element
renderAction fs j =
  let content = Field.Content "" (Field.Selection 0 0 Field.Forward)
      foo a f x = Just (a, f, x)
      rendField name f = case f of
                             Json.Object d -> (case Dict.get "name" d of
                                                   Just (Json.String s) -> (plainText (concat [s, ": "]),
                                                                            Field.field Field.defaultStyle actionFieldInp.handle (foo name s) "" (getContent name s fs))
                                                   _                    -> (plainText "rendField-inner, nope :(", empty))
                             _                  -> (plainText "rendField, nope :(", empty)
      rend d = case (Dict.get "name" d, Dict.get "fields" d) of
                   (Just (Json.String s), Just (Json.Array l))  -> Gfx.bordered Color.darkGray (above (plainText s) (Gfx.renderKV (map (rendField s) l)))
                   _                                            -> plainText "rend, nope :("
  in case j of
         Json.Object d -> beside (rend d) (Input.button handle Nothing "boop")
         _             -> plainText "renderAction, nope :("

renderActions : Dict.Dict String (Dict.Dict String Field.Content) -> Json.Value -> Element
renderActions fs j =
    case j of
        Json.Array l -> flow down (map (renderAction fs) l)
        _            -> plainText "renderActions, nope :("

main : Signal Element
main = let hed  = lift (flow right) (combine [field, constant b])
           stuf = lift2 respToGUI inn actionFieldSig
       in lift (flow down) (combine [lift2 Gfx.centered Window.width hed,
                                     lift2 Gfx.centered Window.width stuf])
