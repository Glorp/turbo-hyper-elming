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

type Header = (String, String)
type Request = {url : String, verb : String, headers : [Header], body: String}
type Response = {body : String, status : Int, statusText : String, headers : [Header]}
type ReqResp = (Request, Maybe Response)
type FieldDict = Dict.Dict String Field.Content
type ActionFieldDict = Dict.Dict String FieldDict
type JsonDict = Dict.Dict String Json.Value

getReq : Maybe String -> String -> Request
getReq r u = req r u "GET" ""

req : Maybe String -> String -> String -> String -> Request
req r u v b =
  let dhs = [("Content-type", "application/x-www-form-urlencoded"),
             ("accept", "application/vnd.siren+json")]
      hs = case r of
               Just s  -> ("X-Alt-Referer", s) :: dhs
               Nothing -> dhs
  in {url = u, verb = v, headers = hs, body = b}

bodyFrom : FieldDict -> String
bodyFrom d =
    let foo l = map (\(k, c) -> concat[k, "=", c.string]) l
    in join "&" (foo (Dict.toList d))

binput = Input.input ()

b = Input.button binput.handle () "beep"

things : (Signal Element, Input.Handle (Maybe Request), Signal (Maybe Request))
things = let addr = "meh.txt"
             inp = Input.input Nothing
             (field, content) = Gfx.makeField "Thing" addr
             fieldSig = sampleOn binput.signal content
             fieldReqSig = lift (.string >> (getReq Nothing) >> Just) fieldSig
             reqSig = merge fieldReqSig inp.signal
         in (field, inp.handle, reqSig)

(field, handle, reqSig) = things

actionFieldInp = Input.input Nothing

actionFieldSig : Signal ActionFieldDict
actionFieldSig = let foo x d = case x of
                                   Nothing        -> Dict.empty
                                   Just (a, f, c) -> insContent a f c d
                 in foldp foo Dict.empty (merge actionFieldInp.signal (lift (\_ -> Nothing) reqSig))


port out : Signal (Maybe Request)
port out = reqSig

port inn : Signal (Maybe ReqResp)

strToGUI : String -> String -> ActionFieldDict -> Element
strToGUI s ref fs =
    let renderl = [("properties", Gfx.renderJsonBut [("name", Gfx.renderJson),
                                                     ("description", Gfx.renderJson)]),
                   ("links", renderLinks ref),
                   ("actions", renderActions fs ref)]
    in case Json.fromString s of
           Just x -> Gfx.renderJsonBut renderl x
           _      -> plainText (concat ["not soap? :() ", s])

respToGUI : Maybe ReqResp -> ActionFieldDict -> Element
respToGUI x fs =
    case x of
        Nothing           -> empty
        Just (rq, Nothing) -> plainText "..."
        Just (rq, Just r)  -> flow down [(plainText (concat ["Status: " , show (r.status), ", ", r.statusText])),
                                        renderHeaders rq.url r.headers,
                                        strToGUI r.body rq.url fs]

renderHeaders : String -> [Header] -> Element
renderHeaders ref hs =
    let link href text = Gfx.butt handle (Just (getReq (Just ref) href)) (plainText text)
        foo (k, v) = case k of
                         "Location" -> (plainText "Location: ", link v v)
                         _          -> (plainText (concat [k, ": "]), plainText v)
    in Gfx.renderKV (map foo hs)

renderLink : String -> Json.Value -> Element
renderLink ref j =
    let link s e d = beside (Gfx.renderJson (Json.Object d))
                            (Gfx.butt handle (Just (getReq (Just ref) s)) e)
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

renderLinks : String -> Json.Value -> Element
renderLinks ref j = case j of
                        Json.Array l -> flow down (map (renderLink ref) l)
                        _            -> Gfx.renderJson j



insContent : String -> String -> Field.Content -> ActionFieldDict -> ActionFieldDict
insContent a f c d = case Dict.get a d of
                          Just dd -> Dict.insert a (Dict.insert f c dd) d
                          _       -> Dict.insert a (Dict.singleton f c) d

renderAction : String -> String -> String -> String -> FieldDict -> JsonDict -> Element
renderAction name href method ref fs d =
    let foo a f x = Just (a, f, x)
        field s = Field.field Field.defaultStyle actionFieldInp.handle (foo name s) "" (Dict.getOrElse content s fs)
        content = Field.Content "" (Field.Selection 0 0 Field.Forward)
        rendField f = case f of
                          Json.Object d -> (case Dict.get "name" d of
                                                Just (Json.String s) -> (plainText (concat [s, ": "]), field s)
                                                _                    -> (plainText "???", Gfx.renderJson f))
                          _             -> (plainText "weird json :|", Gfx.renderJson f)
    in case Dict.get "fields" d of
           Just (Json.Array l)  -> beside (Gfx.bordered Color.darkGray
                                                        (above (plainText name)
                                                               (Gfx.renderKV (concat [map rendField l,
                                                                                      Gfx.renderD (Dict.remove "fields" d)]))))
                                          (Input.button handle (Just (req (Just ref) href method (bodyFrom fs))) "boop")
           _                    -> beside (Gfx.bordered Color.darkGray
                                                        (above (plainText name)
                                                               (Gfx.renderKV (Gfx.renderD d))))
                                          (Input.button handle (Just (req (Just ref) href method (bodyFrom fs))) "boop")



renderActions : ActionFieldDict -> String -> Json.Value -> Element
renderActions afs ref j =
    let rendD d = case (Dict.get "name" d, Dict.get "href" d, Dict.get "method" d) of
                      (Just (Json.String n), Just (Json.String h), Just (Json.String m)) -> renderAction n
                                                                                                         h
                                                                                                         m
                                                                                                         ref
                                                                                                         (Dict.getOrElse Dict.empty n afs)
                                                                                                         (Dict.remove "name" d)
                      _                                                                  -> Gfx.renderJson (Json.Object d)
        rend a = case a of
                     Json.Object d -> rendD d
                     _             -> Gfx.renderJson a
    in case j of
        Json.Array l -> flow down (map rend l)
        _            -> Gfx.renderJson j

main : Signal Element
main = let hed  = lift (flow right) (combine [field, constant b])
           stuf = lift2 respToGUI inn actionFieldSig
       in lift (flow down) (combine [lift2 Gfx.centered Window.width hed,
                                     lift2 Gfx.centered Window.width stuf])
