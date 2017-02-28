# elm-postgrest

[![Build Status](https://travis-ci.org/john-kelly/elm-postgrest.svg?branch=master)](https://travis-ci.org/john-kelly/elm-postgrest)

A query builder library for PostgREST.

## Example

```elm
import PostgRest as PG

type PokemonSchema
    = PokemonSchema

pokemonSchema =
    PG.schema PokemonSchema
        "pokemon"
        { id = PG.int "id"
        , name = PG.string "name"
        , base_experience = PG.int "base_experience"
        , weight = PG.int "weight"
        , height = PG.int "height"
        }

type alias Pokemon =
    { id : Int
    , name : String
    }

pokemonRequest =
    PG.query pokemonSchema Pokemon
        |> PG.select .id
        |> PG.select .name
        |> PG.many "http://localhost:8000/"
            { filters = [ .id |> PG.lte 151 ]
            , orders = [ PG.asc .id ]
            , limit = PG.noLimit
            }
```
