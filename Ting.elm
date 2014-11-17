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
strToGUI s =
    let renderl = [("properties", Gfx.renderJsonBut [("name", Gfx.renderJson),
                                                     ("description", Gfx.renderJson)]),
                   ("links", renderLinks),
                   ("actions", Gfx.renderJson)]
    in case Json.fromString s of
           Just x -> Gfx.renderJsonBut renderl x
           _      -> plainText (concat ["not soap? :() ", s])

respToGUI : Maybe Response -> Element
respToGUI x =
    case x of
        Nothing -> plainText "..."
        Just r  -> flow down [(plainText (concat ["Status: " , show (r.status), ", ", r.statusText])),
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

actionFieldInp = Input.input Nothing

actionFieldSig = let foo x d = case x of
                                   Nothing        -> Dict.empty
                                   Just (f, c) -> Dict.insert f c d
                 in foldp foo Dict.empty (merge actionFieldInp.signal (lift (\_ -> Nothing) reqSig))

getContent k d = case Dict.get k d of
                          Just x -> x
                          _      -> Field.Content "" (Field.Selection 0 0 Field.Forward)

renderAction fs j =
  let content = Field.Content "" (Field.Selection 0 0 Field.Forward)
      foo f x = Just (f, x)
      rendField name f = case f of
                             Json.Object d -> (case Dict.get "name" d of
                                                   Just (Json.String s) -> beside (plainText (concat [s, ": "]))
                                                                                  (Field.field Field.defaultStyle actionFieldInp.handle (foo (name, s)) "" (getContent (name, s) fs))
                                                   _                    -> plainText "rendField-inner, nope :(")
                             _                  -> plainText "rendField, nope :("
      rend d = case (Dict.get "name" d, Dict.get "fields" d) of
                   (Just (Json.String s), Just (Json.Array l))  -> Gfx.bordered Color.darkGray (flow down (plainText s :: (map (rendField s) l)))
                   _                                            -> plainText "rend, nope :("
  in case j of
         Json.Object d -> beside (rend d) (Input.button handle Nothing "boop")
         _             -> plainText "renderAction, nope :("

renderActions r fs =
    let rend s = case Json.fromString s of
                     Just (Json.Object d) -> (case Dict.get "actions" d of
                                                  Just (Json.Array l) -> flow down (map (renderAction fs) l)
                                                  _                   -> plainText "renderActions, inner, inner, nope :(")
                     _                              -> plainText "renderActions, inner, nope :("
    in case r of
           Just resp -> rend resp.respText
           _         -> empty

main : Signal Element
main = let hed  = lift (flow right) (combine [field, constant b])
           stuf = lift (flow right) (combine [lift respToGUI inn])
           acts = lift2 renderActions inn actionFieldSig
       in lift (flow down) (combine [lift2 Gfx.centered Window.width hed,
                                     lift2 Gfx.centered Window.width stuf,
                                     lift2 Gfx.centered Window.width acts])
