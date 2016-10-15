module Nested.TextInput exposing (..)

import Debouncer
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events as Html exposing (..)


type alias Model =
    { textValue : String
    , settledTextValue : String
    }


init =
    { textValue = ""
    , settledTextValue = ""
    }


type Msg
    = OnTextInput String
    | OnSettle


subscriptions : Model -> Sub Msg
subscriptions model =
    Debouncer.settled 1000 "textBouncer" OnSettle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTextInput text ->
            ( { model | textValue = text }, Debouncer.bounce "textBouncer" )

        OnSettle ->
            ( { model | settledTextValue = model.textValue }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "words...", onInput OnTextInput ] []
        , div [] [ text model.settledTextValue ]
        ]
