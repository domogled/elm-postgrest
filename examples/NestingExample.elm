module Main exposing (..)

import Html
import Http
import PostgRest as PG
import Schema


type alias Session =
    { id : Int
    , location : String
    , start_time : String
    , speaker : Speaker
    }


type alias Speaker =
    { id : Int
    , name : String
    , bio : Maybe String
    }


sessionCmd =
    let
        speakerQuery =
            PG.query Schema.speaker Speaker
                |> PG.select .id
                |> PG.select .name
                |> PG.select .bio
    in
        PG.query Schema.session Session
            |> PG.select .id
            |> PG.select .location
            |> PG.select .start_time
            |> PG.embedOne .speaker speakerQuery
            |> PG.readMany "http://postgrest.herokuapp.com/"
                { filter = [ .location |> PG.not PG.ilike "%russia%" ]
                , order = [ PG.asc .start_time ]
                , limit = Nothing
                , offset = Nothing
                }
            |> Http.send Fetch


main =
    Html.program
        { init = ( { sessions = [] }, sessionCmd )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { sessions : List Session
    }



-- UPDATE


type Msg
    = Fetch (Result Http.Error (List Session))


update msg model =
    case msg of
        Fetch (Ok sessions) ->
            ( { model | sessions = sessions }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view { sessions } =
    toString sessions |> Html.text
