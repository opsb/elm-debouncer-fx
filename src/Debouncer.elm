effect module Debouncer where { command = MyCmd, subscription = MySub } exposing (settled, bounce)

import Dict exposing (Dict)
import Time exposing (Time)
import Task exposing (Task)
import Process


-- TODO
-- * cleanup after bounce


type alias State msg =
    { bouncers : Dict String Time
    , subs : Dict.Dict String (Subscription msg)
    }


type MySub msg
    = Settled Time String msg


type alias Subscription msg =
    { name : String
    , stableAfter : Time
    , tagger : msg
    }


type MyCmd msg
    = Bounce String


type Msg
    = Bounced String Time
    | Check String Time



-- STATE


init : Task never (State msg)
init =
    Task.succeed
        { bouncers = Dict.empty
        , subs = Dict.empty
        }



-- COMMANDS


bounce : String -> Cmd msg
bounce key =
    command (Bounce key)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd =
    case cmd of
        Bounce key ->
            Bounce key



-- SUBSCRIPTIONS


settled : Time -> String -> msg -> Sub msg
settled time key tagger =
    subscription (Settled time key tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        Settled time key tagger ->
            Settled time key (func tagger)


buildSubscriptions : List (MySub msg) -> Dict String (Subscription msg)
buildSubscriptions subs =
    let
        insert sub subscriptions =
            case sub of
                Settled time key tagger ->
                    if subscriptions |> Dict.member key then
                        Debug.crash ("debouncer keys must be globally unique, a duplicate was found for: " ++ key)
                    else
                        subscriptions |> Dict.insert key (Subscription key time tagger)
    in
        subs |> List.foldl insert Dict.empty



-- HANDLE EFFECTS


onEffects :
    Platform.Router msg Msg
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds subs state =
    let
        updatedState =
            { state | subs = buildSubscriptions subs }

        handleCommand cmd =
            case cmd of
                Bounce key ->
                    Time.now
                        |> Task.andThen
                            (\time -> Platform.sendToSelf router (Bounced key time))
    in
        cmds
            |> List.map handleCommand
            |> Task.sequence
            |> Task.andThen (\_ -> Task.succeed updatedState)



-- HANDLE SELF MESSAGES


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case (Debug.log "onSelfMsg" selfMsg) of
        Bounced key time ->
            case state.subs |> Dict.get key of
                Nothing ->
                    Task.succeed state

                Just subscription ->
                    let
                        updatedState =
                            { state | bouncers = state.bouncers |> Dict.insert key time }

                        checkTask =
                            (Process.sleep subscription.stableAfter)
                                |> Task.andThen (always <| Time.now)
                                |> Task.andThen (\time -> Platform.sendToSelf router (Check key time))
                    in
                        (Process.spawn checkTask)
                            |> Task.andThen (always <| Task.succeed updatedState)

        Check key time ->
            case state.subs |> Dict.get key of
                Nothing ->
                    Task.succeed state

                Just subscription ->
                    let
                        isSettled =
                            state.bouncers
                                |> Dict.get key
                                |> Maybe.map (\lastBounce -> lastBounce < (time - subscription.stableAfter))
                                |> Maybe.withDefault True
                    in
                        if isSettled then
                            Platform.sendToApp router subscription.tagger
                                |> Task.andThen (always <| Task.succeed state)
                        else
                            Task.succeed state
