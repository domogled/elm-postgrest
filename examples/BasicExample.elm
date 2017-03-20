module Main exposing (..)

import Html
import Http
import PostgRest as PG
import Schema


type alias Session =
    { id : Int
    , location : String
    , start_time : String
    }


sessionCmd =
    PG.query Schema.session Session
        |> PG.select .id
        |> PG.select .location
        |> PG.select .start_time
        |> PG.readMany "http://postgrest.herokuapp.com/"
            { filter = [ .location >> PG.not PG.ilike "%russia%" ]
            , order = [ .start_time >> PG.asc ]
            , limit = Nothing
            , offset = 0
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
