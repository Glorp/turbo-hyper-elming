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
type Response = {url : String, body : String, status : Int, statusText : String, headers : [Header]}
type ReqResp = (Request, Maybe Response)

data ActionUpdate = UField String String Field.Content | UMethod String String | Reset
data Action = Act (Maybe String) FieldDict
type FieldDict = Dict.Dict String Field.Content
type ActionDict = Dict.Dict String Action
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

actionFieldInp = Input.input Reset

insField : String -> String -> Field.Content -> ActionDict -> ActionDict
insField a f c d = case Dict.get a d of
                       Just (Act m dd) -> Dict.insert a (Act m (Dict.insert f c dd)) d
                       _               -> Dict.insert a (Act Nothing (Dict.singleton f c)) d

insMethod : String -> String -> ActionDict -> ActionDict
insMethod a m d = case Dict.get a d of
                      Just (Act _ dd) -> Dict.insert a (Act (Just m) dd) d
                      _               -> Dict.insert a (Act (Just m) Dict.empty) d

actionFieldSig : Signal ActionDict
actionFieldSig = let foo x d = case x of
                                   Reset         -> Dict.empty
                                   UField a f c  -> insField a f c d
                                   UMethod a m   -> insMethod a m d
                 in foldp foo Dict.empty (merge actionFieldInp.signal (lift (\_ -> Reset) reqSig))


port out : Signal (Maybe Request)
port out = reqSig

port inn : Signal (Maybe ReqResp)

strToGUI : String -> String -> ActionDict -> Element
strToGUI s ref fs =
    let renderl = [("properties", Gfx.renderJsonBut [("name", Gfx.renderJson),
                                                     ("description", Gfx.renderJson)]),
                   ("links", renderLinks ref),
                   ("actions", renderActions fs ref)]
    in case Json.fromString s of
           Just x -> Gfx.renderJsonBut renderl x
           _      -> plainText (concat ["not soap? :() ", s])

respToGUI : Maybe ReqResp -> ActionDict -> Element
respToGUI x fs =
    case x of
        Nothing           -> empty
        Just (_, Nothing) -> plainText "..."
        Just (_, Just r)  -> flow down [(plainText (concat ["Status: " , show (r.status), ", ", r.statusText])),
                                        renderHeaders r.url r.headers,
                                        strToGUI r.body r.url fs]

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

methods : String -> [(String, ActionUpdate)]
methods a = [("GET", UMethod a "GET"),
             ("PUT", UMethod a "PUT"),
             ("POST", UMethod a "POST"),
             ("DELETE", UMethod a "DELETE"),
             ("PATCH", UMethod a "PATCH")]

renderAction : String -> String -> Maybe String -> String -> Action -> JsonDict -> Element
renderAction name href method1 ref (Act method2 fs) d =
    let field s = Field.field Field.defaultStyle actionFieldInp.handle (UField name s) "" (Dict.getOrElse content s fs)
        content = Field.Content "" (Field.Selection 0 0 Field.Forward)
        rendField f = case f of
                          Json.Object d -> (case Dict.get "name" d of
                                                Just (Json.String s) -> (plainText (concat [s, ": "]), field s)
                                                _                    -> (plainText "???", Gfx.renderJson f))
                          _             -> (plainText "weird json :|", Gfx.renderJson f)
        method = case (method1, method2) of
                     (Just m, _) -> m
                     (_, Just m) -> m
                     (_, _)      -> "GET"
        button = Input.button handle (Just (req (Just ref) href method (bodyFrom fs))) "boop"
        rendAct = case method1 of
                      Just _  -> button
                      Nothing -> above (Input.dropDown actionFieldInp.handle (methods name)) button
    in case Dict.get "fields" d of
           Just (Json.Array l)  -> beside (Gfx.bordered Color.darkGray
                                                        (above (plainText name)
                                                               (Gfx.renderKV (concat [map rendField l,
                                                                                      Gfx.renderD (Dict.remove "fields" d)]))))
                                          rendAct
           _                    -> beside (Gfx.bordered Color.darkGray
                                                        (above (plainText name)
                                                               (Gfx.renderKV (Gfx.renderD d))))
                                          rendAct



renderActions : ActionDict -> String -> Json.Value -> Element
renderActions afs ref j =
    let rendD d = case (Dict.get "name" d, Dict.get "href" d, Dict.get "method" d) of
                      (Just (Json.String n), Just (Json.String h), Just (Json.String m)) -> renderAction n
                                                                                                         h
                                                                                                         (Just m)
                                                                                                         ref
                                                                                                         (Dict.getOrElse (Act Nothing Dict.empty) n afs)
                                                                                                         (Dict.remove "name" d)
                      (Just (Json.String n), Just (Json.String h), _)                    -> renderAction n
                                                                                                         h
                                                                                                         Nothing
                                                                                                         ref
                                                                                                         (Dict.getOrElse (Act Nothing Dict.empty) n afs)
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
