module Nested.TextInput exposing (..)

import Debouncer
import Html exposing (..)
import Html.Attributes exposing (..)
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


subscriptions : String -> Model -> Sub Msg
subscriptions bouncer model =
    Debouncer.settled 1000 bouncer OnSettle


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update bouncer msg model =
    case msg of
        OnTextInput text ->
            ( { model | textValue = text }, Debouncer.bounce bouncer )

        OnSettle ->
            ( { model | settledTextValue = model.textValue }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "words...", onInput OnTextInput ] []
        , div [] [ text model.settledTextValue ]
        ]
