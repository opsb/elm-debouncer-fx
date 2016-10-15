effect module Debouncer where { command = MyCmd, subscription = MySub } exposing (settled, bounce)

import Dict exposing (Dict)
import Time exposing (Time)
import Task exposing (Task)
import Process


-- TODO
-- * cleanup after bounce
-- * single subscription per key - still need to throw error
-- TYPES
-- TYPES


type alias State msg =
    { bouncers : Dict String Time
    , subs : Subscriptions msg
    }


type MySub msg
    = Settled Time String msg


type alias Subscriptions msg =
    { settled : SubsDict msg
    }


type alias SubsDict msg =
    Dict.Dict String (Subscription msg)


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
        , subs = emptySubscriptions
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


emptySubscriptions =
    { settled = Dict.empty
    }


buildSubscriptions : List (MySub msg) -> Subscriptions msg
buildSubscriptions subs =
    let
        addSubscription key subscription settledSubscriptions =
            if settledSubscriptions |> Dict.member key then
                Debug.crash ("debouncer keys must be globally unique, a duplicate was found for: " ++ key)
            else
                settledSubscriptions |> Dict.insert key subscription

        insert sub subscriptions =
            case sub of
                Settled time key tagger ->
                    { subscriptions
                        | settled =
                            subscriptions.settled |> addSubscription key (Subscription key time tagger)
                    }
    in
        subs |> List.foldl insert emptySubscriptions



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
                        |> taskAndThen
                            (\time -> Platform.sendToSelf router (Bounced key time))
    in
        cmds
            |> List.map handleCommand
            |> Task.sequence
            |> taskAndThen (\_ -> Task.succeed updatedState)


findSubscription key state =
    case state.subs.settled |> Dict.get key of
        Just subscription ->
            subscription

        Nothing ->
            Debug.crash ("not settled subscription found for " ++ key)



-- HANDLE SELF MESSAGES


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case (Debug.log "onSelfMsg" selfMsg) of
        Bounced key time ->
            let
                stableAfter =
                    findSubscription key state |> .stableAfter

                updatedState =
                    { state | bouncers = state.bouncers |> Dict.insert key time }

                checkTask =
                    (Process.sleep stableAfter)
                        |> taskAndThen (always <| Time.now)
                        |> taskAndThen (\time -> Platform.sendToSelf router (Check key time))
            in
                (Process.spawn checkTask)
                    |> taskAndThen (always <| Task.succeed updatedState)

        Check key time ->
            let
                stableAfter =
                    findSubscription key state |> .stableAfter

                isSettled =
                    state.bouncers
                        |> Dict.get key
                        |> Maybe.map (\lastBounce -> lastBounce < (time - stableAfter))
                        |> Maybe.withDefault True

                subscription =
                    state.subs.settled
                        |> Dict.get key
            in
                case ( isSettled, subscription ) of
                    ( True, Just subscription ) ->
                        Platform.sendToApp router subscription.tagger
                            |> taskAndThen (always <| Task.succeed state)

                    _ ->
                        Task.succeed state



-- HELPERS


taskAndThen =
    flip Task.andThen
