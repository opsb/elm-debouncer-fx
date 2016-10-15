# Elm-Debouncer-Fx

An effect manager for debouncing.

```Elm
type alias Model =
    { textValue : String
    , settledTextValue : String
    }


init =
    ( { textValue = ""
      , settledTextValue = ""
      }
    , Cmd.none
    )


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

```
