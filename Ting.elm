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
import Text

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

jsonGet : Json.Value -> String -> Maybe Json.Value
jsonGet j s = case j of
                  Json.Object d -> Dict.get s d
                  _             -> Nothing

binput = Input.input ()

b = Input.button binput.handle () "beep"

things : (Signal Element, Input.Handle (Maybe Request), Signal (Maybe Request))
things = let addr = "http://hyperwizard.azurewebsites.net/hywit/void"
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

renderJ : Json.Value -> Gfx.Elem
renderJ = Gfx.renderJson >> Gfx.Labeled
renderJBut : [(String, Json.Value -> Gfx.Elem)] -> Json.Value -> Gfx.Elem
renderJBut l = Gfx.renderJsonBut l >> Gfx.Labeled

renderTitle : Json.Value -> Gfx.Elem
renderTitle j = case j of
                    Json.String s -> Gfx.Only (Text.leftAligned (Text.bold (Text.height 14 (Text.toText s))))
                    _             -> Gfx.Labeled (Gfx.renderJson j)

strToGUI : String -> String -> ActionDict -> Element
strToGUI s ref fs =
    let renderl = [("title",  renderTitle),
                   ("properties", renderJBut [("name", renderJ),
                                              ("description", renderJ)]),
                   ("links", renderLinks ref >> Gfx.Labeled),
                   ("actions", renderActions fs ref >> Gfx.Labeled)]
    in case Json.fromString s of
           Just x -> Gfx.renderJsonBut renderl x
           _      -> let e = plainText s
                     in if widthOf e > 500 then width 500 e else e

rendStatus r =
    let ott = fittedImage 100 100 "sea-otter.jpg"
        rs = if r.status == 303 then ott else plainText r.statusText
        ls = plainText (concat ["Status: " , show (r.status), ", "])
    in beside (container (widthOf ls) (heightOf rs) middle ls) rs

respToGUI : Maybe ReqResp -> ActionDict -> Element
respToGUI x fs =
    case x of
        Nothing           -> empty
        Just (_, Nothing) -> plainText "..."
        Just (_, Just r)  -> flow down [rendStatus r,
                                        renderHeaders r.url r.headers,
                                        strToGUI r.body r.url fs]
liink ref href s =
    let t = Text.toText s
    in Input.customButton handle
                          (Just (getReq (Just ref) href))
                          (Text.leftAligned (Text.color Color.lightBlue t))
                          (Text.leftAligned  (Text.color Color.blue t))
                          (Text.leftAligned  (Text.color Color.darkBlue t))

renderHeaders : String -> [Header] -> Element
renderHeaders ref hs =
    let foo (k, v) = case k of
                         "Location" -> ("Location: ", Gfx.Labeled (liink ref v v))
                         _          -> (concat [k, ": "], Gfx.Labeled (plainText v))
    in Gfx.renderKV (map foo hs)

relToString : [Json.Value] -> Maybe String
relToString l =
    let foo j r = case (j, r) of
                      (Json.String s, Just "") -> Just s
                      (Json.String s, Just rs) -> Just (concat [rs, " ", s])
                      _                        -> Nothing
    in foldl foo (Just "") l


renderLink : String -> Json.Value -> Element
renderLink ref j =
    let text h = case (jsonGet j "title", jsonGet j "rel") of
                     (Just (Json.String t), _) -> t
                     (_, Just (Json.Array l))  -> (case relToString l of
                                                       Just s -> s
                                                       _      -> h)
                     _                          -> h
        rendLink h = liink ref h (text h)
        rendImg h = Gfx.bordered Color.darkGray
                                 (above (leftAligned (Text.link h (Text.toText (text h))))
                                        (fittedImage 400 400 h)) 
    in case (jsonGet j "href", jsonGet j "type") of
           (Just (Json.String h), Just (Json.String s)) -> if String.slice 0 6 s == "image/"
                                                           then rendImg h
                                                           else rendLink h
           (Just (Json.String h), _)                    -> rendLink h
           _                                            -> Gfx.renderJson j

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

renderAction : String -> String -> Maybe String -> String -> Action -> Json.Value -> Element
renderAction name href method1 ref (Act method2 fs) j =
    let field s = Field.field Field.defaultStyle actionFieldInp.handle (UField name s) "" (Dict.getOrElse content s fs)
        content = Field.Content "" (Field.Selection 0 0 Field.Forward)
        rendField f = case jsonGet f "name" of
                          Just (Json.String s) -> (concat [s, ": "], Gfx.Labeled (field s))
                          _                    -> ("???", Gfx.Labeled (Gfx.renderJson f))
        rendFields fs = case fs of
                              Json.Array l -> Gfx.bordered Color.lightGrey (Gfx.renderKV (map rendField l))
                              _            -> Gfx.renderJson fs
        method = case (method1, method2) of
                     (Just m, _) -> m
                     (_, Just m) -> m
                     (_, _)      -> "GET"
        button = Input.button handle (Just (req (Just ref) href method (bodyFrom fs))) "boop"
        rendAct = case method1 of
                      Just _  -> button
                      Nothing -> above (Input.dropDown actionFieldInp.handle (methods name)) button

    in beside (Gfx.renderJsonBut [("title", renderTitle),
                                  ("name", renderJ),
                                  ("href", renderJ),
                                  ("method", renderJ),
                                  ("fields", rendFields >> Gfx.Labeled)]
                                 j)
              rendAct



renderActions : ActionDict -> String -> Json.Value -> Element
renderActions afs ref j =
    let noAct = Act Nothing Dict.empty
        method a = case jsonGet a "method" of
                       Just (Json.String m) -> Just m
                       _                    -> Nothing
        rend a = case (jsonGet a "name", jsonGet a "href") of
                     (Just (Json.String n), Just (Json.String h)) -> renderAction n
                                                                                  h
                                                                                  (method a)
                                                                                  ref
                                                                                  (Dict.getOrElse noAct n afs)
                                                                                  a
                     _                                           -> Gfx.renderJson a
    in case j of
        Json.Array l -> flow down (map rend l)
        _            -> Gfx.renderJson j

main : Signal Element
main = let hed  = lift (flow right) (combine [field, constant b])
           stuf = lift2 respToGUI inn actionFieldSig
       in lift (flow down) (combine [lift2 Gfx.centered Window.width hed,
                                     lift2 Gfx.centered Window.width stuf])
