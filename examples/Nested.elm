module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events as Html exposing (..)
import Nested.TextInput


main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { textInputA : Nested.TextInput.Model
    , textInputB : Nested.TextInput.Model
    }


init =
    ( { textInputA = Nested.TextInput.init
      , textInputB = Nested.TextInput.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TextInputAMsg (Nested.TextInput.subscriptions model.textInputA)
        , Sub.map TextInputBMsg (Nested.TextInput.subscriptions model.textInputB)
        ]


type Msg
    = TextInputAMsg Nested.TextInput.Msg
    | TextInputBMsg Nested.TextInput.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextInputAMsg textInputMsg ->
            let
                ( updatedTextInputA, cmd ) =
                    Nested.TextInput.update textInputMsg model.textInputA
            in
                ( { model | textInputA = updatedTextInputA }
                , Cmd.map TextInputAMsg cmd
                )

        TextInputBMsg textInputMsg ->
            let
                ( updatedTextInputB, cmd ) =
                    Nested.TextInput.update textInputMsg model.textInputB
            in
                ( { model | textInputB = updatedTextInputB }
                , Cmd.map TextInputBMsg cmd
                )


view : Model -> Html Msg
view model =
    div []
        [ Html.App.map TextInputAMsg (Nested.TextInput.view model.textInputA)
        , Html.App.map TextInputBMsg (Nested.TextInput.view model.textInputB)
        ]
