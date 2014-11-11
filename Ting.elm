module Ting where
import Graphics.Input.Field as Field
import Graphics.Input as Input
import String
import Maybe
import Http
import Json


id x = x

makeField s ss = let content = Input.input (Field.Content ss (Field.Selection 0 0 Field.Forward))
                     fi = lift (Field.field Field.defaultStyle content.handle id s) content.signal
                 in (fi, content.signal)

binput = Input.input ()

b = Input.button binput.handle () "Knapp"

(field, handle, reqSig) = let inp = Input.input Nothing
                              (field, content) = makeField "λ" "meh.txt"
                              fieldSig = sampleOn binput.signal content
                              fieldReqSig = lift (.string >> Http.get) fieldSig
                              foo fieldV v = case v of
                                                 Just s  -> s
                                                 Nothing -> fieldV
                              reqSig = lift2 foo fieldReqSig inp.signal
                          in (field, inp.handle, reqSig)



res = Http.send reqSig

respToStr x = case x of
                 Http.Success s   -> s
                 Http.Waiting     -> ":|"
                 Http.Failure i s -> String.append ":((( " s

strToGUI s = let foo x = case x of
                        Json.String s -> Input.button handle (Just (Http.get s)) s
                        _             -> plainText ":/"
        in case Json.fromString s of
               Just (Json.Array x) -> flow down (map foo x)
               _                   -> plainText (String.concat [":( -- ", s])


main = lift (flow right) (combine [field, constant b, lift (respToStr >> strToGUI) res])
