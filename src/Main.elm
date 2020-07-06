module Main exposing(main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import Http
import Regex
import List
import Json.Encode as Encode exposing(string)


jsonBody : String -> Http.Body
jsonBody value =
  Http.stringBody "application/json" (Encode.encode 0 (string value))


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL
type Methods = Get 
             | Post
             | Put
             | Delete

methodsConvertToString : Methods -> String
methodsConvertToString methodsInfo = 
  case methodsInfo of
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"

type alias Model = { 
      methodName: Methods,
      urlText: String,
      answerResult: String,
      requestBody: String,
      isRequest: Bool,
      headers: String
    } 

init: () -> (Model, Cmd Msg)
init _ = ({
    methodName = Get ,
    urlText = "" ,
    answerResult = "" ,
    isRequest = False ,
    headers = "",
    requestBody = ""
  }, Cmd.none)
-- UPDATE

type Msg = ChangeMethod
         | ChangeHeaders String
         | ChangeUrl String
         | ChangeBodyRequest String
         | ResultRequest (Result Http.Error String)
         | ResetResult
         | Submit

headersRegex : Regex.Regex
headersRegex =
  Maybe.withDefault Regex.never <|
    Regex.fromString "\"(\\w+)\" \"(\\w+)\""
-- VIEW

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeBodyRequest body -> ({
      model | requestBody = body
      }, Cmd.none)
    ChangeHeaders headers -> ({
      model | headers = headers
      }, Cmd.none)
    ResetResult -> ({
      model | answerResult = ""
      }, Cmd.none)
    Submit -> 
      ({
        model | isRequest = True
      }, requestToUrl model.methodName model.urlText model.headers model.requestBody)
    ChangeUrl url -> ({
        model | urlText = url
      }, Cmd.none)
    ChangeMethod -> 
      case model.methodName of
        Get -> ({
            model | methodName = Post
          }, Cmd.none)
        Post -> ({
            model | methodName = Put
          }, Cmd.none)
        Put -> ({
            model | methodName = Delete
          }, Cmd.none)
        Delete -> ({
            model | methodName = Get
          }, Cmd.none)
    ResultRequest result -> 
      case result of
        Ok textAnswer -> ({
            model | isRequest = False ,
            answerResult = textAnswer
            }, Cmd.none)
        Err _ -> ({
            model | isRequest = False ,
            answerResult = "Error request"
            }, Cmd.none)

requestToUrl : Methods -> String -> String -> String -> Cmd Msg
requestToUrl methods url headers body = Http.request
    { method = methodsConvertToString methods
    , headers = headerOfString headers
    , url = url
    , body = jsonBody body
    , expect = Http.expectString ResultRequest
    , timeout = Nothing
    , tracker = Nothing
    }
    
validateArray : List String -> (String, String)
validateArray array = (
  case List.head array of 
    Just a -> a
    Nothing -> "none"
  ,
  case List.tail array of 
    Just a -> 
      case List.head a of
        Just w -> w
        Nothing -> "none"
    Nothing -> "none"
  )

headerOfString : String -> List Http.Header
headerOfString infoHeaders = List.map (\keyAndValue -> let info = validateArray keyAndValue in
  Http.header (Tuple.first info) (Tuple.second info) ) 
    (List.map (\x -> List.map (\y -> case y of 
        Just a -> a 
        Nothing -> "")
      x.submatches) (Regex.find headersRegex infoHeaders))

view : Model -> Html Msg
view model =
  div [ style "display" "flex" ,
        style "justify-content" "center" ,
        style "align-items" "center" ,
        style "flex-direction" "column" ,
        style "padding" "15px 0px" ,
        style "font-family" "sans-serif"
      ]
    [ 
      div [ style "margin-bottom" "10px" ] [
        input [ style "width" "300", value model.urlText, placeholder "Input url", onInput ChangeUrl  ] [] ,
        button [ onClick Submit] [ text "send" ]
      ] ,
      div [ style "text-align" "center" ] [
        div [ style "min-height" "80px" ] [
          textarea [
            style "width" "320px" ,
            style "margin-bottom" "10px" ,
            placeholder "headers -> \"key\" \"value\"" ,
            onInput ChangeHeaders
          ] [] ,
          textarea [ style "display" (if model.methodName == Post || model.methodName == Put then "block" else "none") ,
                     style "margin-bottom" "10px" ,
                     style "width" "320px" ,
                     placeholder "input date" ,
                     onInput ChangeBodyRequest
                 ] []
        ] ,
        div [] [ text (methodsConvertToString model.methodName) ] ,
        div [] [
          button [ style "margin-top" "10px", onClick ChangeMethod] [ text "change method" ]
        ]
      ] ,
      div [ style "margin-top" "10px" ] [
        button [ style "margin" "0px auto",
                 style "display" (if String.length model.answerResult > 0 then "block" else "none"),
                 style "margin-bottom" "10px", 
                 onClick ResetResult ] 
               [ text "delete answer request"] ,
        div [ style "text-align" "center" ] [
          text (if model.isRequest then "loading..." else model.answerResult)
        ]
      ]
    ]