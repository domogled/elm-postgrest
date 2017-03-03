module PostgRest
    exposing
        ( Field
        , Schema
        , Query
        , OrderBy
        , Filter
        , Page
        , Relationship
        , HasOne
        , HasOneNullable
        , HasMany
        , hasOne
        , hasOneNullable
        , hasMany
        , field
        , string
        , int
        , float
        , bool
        , nullable
        , schema
        , query
        , embedOne
        , embedOneNullable
        , embedMany
        , select
        , hardcoded
        , like
        , eq
        , gte
        , gt
        , lte
        , lt
        , ilike
        , inList
        , is
        , not
        , asc
        , desc
        , requestMany
        , requestOne
        , requestPage
        )

{-| A query builder library for PostgREST.

I recommend looking at the [examples](https://github.com/john-kelly/elm-postgrest/blob/master/examples/Main.elm) before diving into the API or source code.

# Define a Schema
@docs Schema, schema

### Fields
@docs Field, string, int, float, bool, field, nullable

### Relationships
@docs Relationship, HasOne, hasOne, HasOneNullable, hasOneNullable, HasMany, hasMany

# Build a Query
@docs Query, query

### Selecting and Nesting
@docs select, embedOne, embedOneNullable, embedMany, hardcoded

### Filtering
@docs Filter, like, ilike, eq, gte, gt, lte, lt, inList, is, not

### Ordering
@docs OrderBy, asc, desc

# Send a Query
@docs requestMany, requestOne

### Pagination
@docs Page, requestPage

-}

import Dict
import Http
import Json.Decode as Decode
import Regex
import String


{-| -}
type Schema id fields
    = Schema String fields


{-| -}
type Query id fields a
    = Query fields Parameters (Decode.Decoder a)


{-| -}
type Parameters
    = Parameters
        { name : String
        , select : List String
        , order : List OrderBy
        , filter : List Filter
        , limit : Maybe Int
        , offset : Maybe Int
        , embedded : List Parameters
        }


{-| -}
type alias Page a =
    { data : List a
    , count : Int
    }


{-| -}
type Field a
    = Field (Decode.Decoder a) (a -> String) String


{-| -}
type OrderBy
    = Asc String
    | Desc String


{-| -}
type Condition
    = Like String
    | ILike String
    | Eq String
    | Gte String
    | Gt String
    | Lte String
    | Lt String
    | In (List String)
    | Is String


{-| -}
type Filter
    = Filter Bool Condition String



-- https://wiki.haskell.org/Empty_type
-- https://wiki.haskell.org/Phantom_type


{-| -}
type HasMany
    = HasMany HasMany


{-| -}
type HasOne
    = HasOne HasOne


{-| -}
type HasOneNullable
    = HasOneNullable HasOneNullable


{-| -}
type Relationship a id
    = Relationship


{-| -}
hasOne : id -> Relationship HasOne id
hasOne id =
    Relationship


{-| -}
hasMany : id -> Relationship HasMany id
hasMany id =
    Relationship


{-| -}
hasOneNullable : id -> Relationship HasOneNullable id
hasOneNullable id =
    Relationship


{-| -}
schema : id -> String -> fields -> Schema id fields
schema id name s =
    Schema name s


{-| -}
field : Decode.Decoder a -> (a -> String) -> String -> Field a
field =
    Field


{-| -}
int : String -> Field Int
int =
    Field Decode.int toString


{-| -}
string : String -> Field String
string =
    Field Decode.string identity


{-| -}
float : String -> Field Float
float =
    Field Decode.float toString


{-| -}
bool : String -> Field Bool
bool =
    Field Decode.bool toString


{-| -}
nullable : Field a -> Field (Maybe a)
nullable (Field decoder urlEncoder name) =
    let
        fieldToString maybeVal =
            case maybeVal of
                Just val ->
                    urlEncoder val

                Nothing ->
                    "null"
    in
        Field (Decode.nullable decoder) fieldToString name


{-| -}
query : Schema id fields -> (a -> b) -> Query id fields (a -> b)
query (Schema name fields) ctor =
    Query fields
        (Parameters
            { name = name
            , select = []
            , filter = []
            , order = []
            , limit = Nothing
            , offset = Nothing
            , embedded = []
            }
        )
        (Decode.succeed ctor)


{-| -}
embedOne :
    (fields1 -> Relationship HasOne id2)
    -> Query id2 fields2 a
    -> Query id1 fields1 (a -> b)
    -> Query id1 fields1 b
embedOne _ (Query _ (Parameters subParams) subDecoder) (Query fields (Parameters params) decoder) =
    Query fields
        (Parameters { params | embedded = (Parameters subParams) :: params.embedded })
        (apply decoder (Decode.field subParams.name subDecoder))


{-| -}
embedOneNullable :
    (fields1 -> Relationship HasOneNullable id2)
    -> Query id2 fields2 a
    -> Query id1 fields1 (Maybe a -> b)
    -> Query id1 fields1 b
embedOneNullable _ (Query _ (Parameters subParams) subDecoder) (Query fields (Parameters params) decoder) =
    Query fields
        (Parameters { params | embedded = (Parameters subParams) :: params.embedded })
        (apply decoder (Decode.field subParams.name (Decode.nullable subDecoder)))


{-| -}
embedMany :
    (fields1 -> Relationship HasMany id2)
    -> Query id2 fields2 a
    -> { filters : List (fields2 -> Filter)
       , order : List (fields2 -> OrderBy)
       , limit : Maybe Int
       , offset : Maybe Int
       }
    -> Query id1 fields1 (List a -> b)
    -> Query id1 fields1 b
embedMany _ (Query subFields (Parameters subParams) subDecoder) options (Query fields (Parameters params) decoder) =
    let
        newSubParams =
            { subParams
                | filter = List.map (\getFilter -> getFilter subFields) options.filters
                , order = List.map (\getOrder -> getOrder subFields) options.order
                , limit = options.limit
                , offset = options.offset
            }
    in
        Query fields
            (Parameters { params | embedded = Parameters newSubParams :: params.embedded })
            (apply decoder (Decode.field newSubParams.name (Decode.list subDecoder)))


{-| -}
select : (fields -> Field a) -> Query id fields (a -> b) -> Query id fields b
select getField (Query fields (Parameters params) queryDecoder) =
    case getField fields of
        Field fieldDecoder _ fieldName ->
            Query fields
                (Parameters { params | select = fieldName :: params.select })
                (apply queryDecoder (Decode.field fieldName fieldDecoder))


{-| -}
hardcoded : a -> Query id fields (a -> b) -> Query id fields b
hardcoded val (Query fields params queryDecoder) =
    Query fields
        params
        (apply queryDecoder (Decode.succeed val))


{-| -}
singleValueFilterFn : (String -> Condition) -> a -> (fields -> Field a) -> fields -> Filter
singleValueFilterFn condCtor condArg getField fields =
    case getField fields of
        Field _ urlEncoder name ->
            Filter False (condCtor (urlEncoder condArg)) name


{-|
Simple [pattern matching](https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-LIKE)
-}
like : String -> (fields -> Field String) -> fields -> Filter
like =
    singleValueFilterFn Like


{-| Case-insensitive `like`
-}
ilike : String -> (fields -> Field String) -> fields -> Filter
ilike =
    singleValueFilterFn ILike


{-| Equals
-}
eq : a -> (fields -> Field a) -> fields -> Filter
eq =
    singleValueFilterFn Eq


{-| Greater than or equal to
-}
gte : a -> (fields -> Field a) -> fields -> Filter
gte =
    singleValueFilterFn Gte


{-| Greater than
-}
gt : a -> (fields -> Field a) -> fields -> Filter
gt =
    singleValueFilterFn Gt


{-| Less than or equal to
-}
lte : a -> (fields -> Field a) -> fields -> Filter
lte =
    singleValueFilterFn Lte


{-| Less than
-}
lt : a -> (fields -> Field a) -> fields -> Filter
lt =
    singleValueFilterFn Lt


{-| In List
-}
inList : List a -> (fields -> Field a) -> fields -> Filter
inList condArgs getField fields =
    case getField fields of
        Field _ urlEncoder name ->
            Filter False (In (List.map urlEncoder condArgs)) name


{-| Is comparison
-}
is : a -> (fields -> Field a) -> fields -> Filter
is =
    singleValueFilterFn Is


{-| Negate a Filter
-}
not :
    (a -> (fields -> Field a) -> fields -> Filter)
    -> a
    -> (fields -> Field a)
    -> fields
    -> Filter
not filterCtor val getField fields =
    case filterCtor val getField fields of
        Filter negated cond fieldName ->
            Filter (Basics.not negated) cond fieldName


{-| Ascending
-}
asc : (fields -> Field a) -> fields -> OrderBy
asc getField fields =
    case getField fields of
        Field _ _ name ->
            Asc name


{-| Descending
-}
desc : (fields -> Field a) -> fields -> OrderBy
desc getField fields =
    case getField fields of
        Field _ _ name ->
            Desc name


{-| -}
requestMany :
    String
    -> { filters : List (fields -> Filter)
       , order : List (fields -> OrderBy)
       , limit : Maybe Int
       , offset : Maybe Int
       }
    -> Query id fields a
    -> Http.Request (List a)
requestMany url options (Query fields (Parameters params) decoder) =
    let
        newParams =
            { params
                | filter = List.map (\getFilter -> getFilter fields) options.filters
                , order = List.map (\getOrder -> getOrder fields) options.order
                , limit = options.limit
                , offset = options.offset
            }

        settings =
            { count = False
            , singular = False
            }

        ( headers, queryUrl ) =
            getHeadersAndQueryUrl settings
                url
                newParams.name
                (Parameters newParams)
    in
        Http.request
            { method = "GET"
            , headers = headers
            , url = queryUrl
            , body = Http.emptyBody
            , expect = Http.expectJson (Decode.list decoder)
            , timeout = Nothing
            , withCredentials = False
            }


{-| Get single row. Http Error if many or no rows are returned.
-}
requestOne :
    String
    -> List (fields -> Filter)
    -> Query id fields a
    -> Http.Request a
requestOne url filters (Query fields (Parameters params) decoder) =
    let
        newParams =
            { params | filter = List.map (\getFilter -> getFilter fields) filters }

        settings =
            { count = False
            , singular = True
            }

        ( headers, queryUrl ) =
            getHeadersAndQueryUrl settings
                url
                newParams.name
                (Parameters newParams)
    in
        Http.request
            { method = "GET"
            , headers = headers
            , url = queryUrl
            , body = Http.emptyBody
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }


{-| -}
requestPage :
    String
    -> { filters : List (fields -> Filter)
       , order : List (fields -> OrderBy)
       , page : Int
       , size : Int
       }
    -> Query id fields a
    -> Http.Request (Page a)
requestPage url options (Query fields (Parameters params) decoder) =
    let
        newParams =
            { params
                | filter = List.map (\getFilter -> getFilter fields) options.filters
                , order = List.map (\getOrder -> getOrder fields) options.order
                , limit = (Just options.size)
                , offset = Just ((options.page - 1) * options.size)
            }

        settings =
            { count = True
            , singular = False
            }

        ( headers, queryUrl ) =
            getHeadersAndQueryUrl settings
                url
                newParams.name
                (Parameters newParams)

        handleResponse response =
            let
                countResult =
                    Dict.get "Content-Range" response.headers
                        |> Result.fromMaybe "No Content-Range Header"
                        |> Result.andThen (Regex.replace (Regex.All) (Regex.regex ".+\\/") (always "") >> Ok)
                        |> Result.andThen String.toInt

                jsonResult =
                    Decode.decodeString (Decode.list decoder) response.body
            in
                Result.map2 Page jsonResult countResult
    in
        Http.request
            { method = "GET"
            , headers = headers
            , url = queryUrl
            , body = Http.emptyBody
            , expect = Http.expectStringResponse handleResponse
            , timeout = Nothing
            , withCredentials = False
            }


type alias Settings =
    { count : Bool
    , singular : Bool
    }


getHeadersAndQueryUrl : Settings -> String -> String -> Parameters -> ( List Http.Header, String )
getHeadersAndQueryUrl settings url name p =
    let
        { count, singular } =
            settings

        trailingSlashUrl =
            if String.right 1 url == "/" then
                url
            else
                url ++ "/"

        ( labeledOrders, labeledFilters, labeledLimits, labeledOffset ) =
            labelParams p

        queryUrl =
            [ selectsToKeyValue p
            , labeledFiltersToKeyValues labeledFilters
            , labeledOrdersToKeyValue labeledOrders
            , labeledMaybeIntToKeyValue "limit" labeledLimits
            , labeledMaybeIntToKeyValue "offset" labeledOffset
            ]
                |> List.foldl (++) []
                |> queryParamsToUrl (trailingSlashUrl ++ name)

        pluralityHeader =
            if singular then
                -- https://postgrest.com/en/v0.4/api.html#singular-or-plural
                [ ( "Accept", "application/vnd.pgrst.object+json" ) ]
            else
                []

        countHeader =
            if count then
                -- https://github.com/begriffs/postgrest/pull/700
                [ ( "Prefer", "count=exact" ) ]
            else
                []

        headers =
            (pluralityHeader ++ countHeader)
                |> List.map (\( a, b ) -> Http.header a b)
    in
        ( headers, queryUrl )


selectsToKeyValueHelper : Parameters -> String
selectsToKeyValueHelper (Parameters params) =
    let
        embedded =
            List.map selectsToKeyValueHelper params.embedded

        selection =
            String.join "," (params.select ++ embedded)
    in
        params.name ++ "{" ++ selection ++ "}"


selectsToKeyValue : Parameters -> List ( String, String )
selectsToKeyValue (Parameters params) =
    let
        embedded =
            List.map selectsToKeyValueHelper params.embedded

        selection =
            String.join "," (params.select ++ embedded)
    in
        [ ( "select", selection ) ]


labelParamsHelper :
    String
    -> Parameters
    -> ( List ( String, OrderBy ), List ( String, Filter ), List ( String, Maybe Int ), List ( String, Maybe Int ) )
labelParamsHelper prefix (Parameters params) =
    let
        labelWithPrefix : a -> ( String, a )
        labelWithPrefix =
            (,) prefix

        labelNested : Parameters -> ( List ( String, OrderBy ), List ( String, Filter ), List ( String, Maybe Int ), List ( String, Maybe Int ) )
        labelNested (Parameters params) =
            labelParamsHelper (prefix ++ params.name ++ ".") (Parameters params)

        appendTriples :
            ( appendable1, appendable2, appendable3, appendable4 )
            -> ( appendable1, appendable2, appendable3, appendable4 )
            -> ( appendable1, appendable2, appendable3, appendable4 )
        appendTriples ( os1, fs1, ls1, of1 ) ( os2, fs2, ls2, of2 ) =
            ( os1 ++ os2
            , fs1 ++ fs2
            , ls1 ++ ls2
            , of1 ++ of2
            )

        labeledOrders : List ( String, OrderBy )
        labeledOrders =
            List.map labelWithPrefix params.order

        labeledFilters : List ( String, Filter )
        labeledFilters =
            List.map labelWithPrefix params.filter

        labeledLimit : List ( String, Maybe Int )
        labeledLimit =
            [ labelWithPrefix params.limit ]

        labeledOffset : List ( String, Maybe Int )
        labeledOffset =
            [ labelWithPrefix params.offset ]
    in
        params.embedded
            |> List.map labelNested
            |> List.foldl appendTriples
                ( labeledOrders
                , labeledFilters
                , labeledLimit
                , labeledOffset
                )


{-| NOTE: What if we were to label when we add?
OrderBy, Filter, and Limit could have a List String which is populated by prefix info whenever
a query is embedded in another query. We would still need an operation to flatten
the QueryParams, but the logic would be much simpler (would no longer be a weird
concatMap) This may be a good idea / improve performance a smudge (prematureoptimzation much?)
-}
labelParams : Parameters -> ( List ( String, OrderBy ), List ( String, Filter ), List ( String, Maybe Int ), List ( String, Maybe Int ) )
labelParams =
    labelParamsHelper ""


labeledFiltersToKeyValues : List ( String, Filter ) -> List ( String, String )
labeledFiltersToKeyValues filters =
    let
        contToString : Condition -> String
        contToString cond =
            case cond of
                Like str ->
                    "like." ++ str

                Eq str ->
                    "eq." ++ str

                Gte str ->
                    "gte." ++ str

                Gt str ->
                    "gt." ++ str

                Lte str ->
                    "lte." ++ str

                Lt str ->
                    "lt." ++ str

                ILike str ->
                    "ilike." ++ str

                In list ->
                    "in." ++ String.join "," list

                Is str ->
                    "is." ++ str

        filterToKeyValue : ( String, Filter ) -> ( String, String )
        filterToKeyValue ( prefix, filter ) =
            case filter of
                Filter True cond key ->
                    ( prefix ++ key, "not." ++ contToString cond )

                Filter False cond key ->
                    ( prefix ++ key, contToString cond )
    in
        List.map filterToKeyValue filters


labeledOrdersToKeyValue : List ( String, OrderBy ) -> List ( String, String )
labeledOrdersToKeyValue orders =
    let
        orderToString : OrderBy -> String
        orderToString order =
            case order of
                Asc name ->
                    name ++ ".asc"

                Desc name ->
                    name ++ ".desc"

        labeledOrderToKeyValue : ( String, List OrderBy ) -> Maybe ( String, String )
        labeledOrderToKeyValue ( prefix, orders ) =
            case orders of
                [] ->
                    Nothing

                _ ->
                    Just
                        ( prefix ++ "order"
                        , orders
                            |> List.map orderToString
                            |> String.join ","
                        )
    in
        orders
            |> List.foldr
                (\( prefix, order ) dict ->
                    Dict.update prefix
                        (\maybeOrders ->
                            case maybeOrders of
                                Nothing ->
                                    Just [ order ]

                                Just os ->
                                    Just (order :: os)
                        )
                        dict
                )
                Dict.empty
            |> Dict.toList
            |> List.filterMap labeledOrderToKeyValue


labeledMaybeIntToKeyValue : String -> List ( String, Maybe Int ) -> List ( String, String )
labeledMaybeIntToKeyValue option list =
    let
        toKeyValue : ( String, Maybe Int ) -> Maybe ( String, String )
        toKeyValue labeled =
            case labeled of
                ( _, Nothing ) ->
                    Nothing

                ( prefix, Just v ) ->
                    Just ( prefix ++ option, toString v )
    in
        List.filterMap toKeyValue list


{-| Copy pasta of the old Http.url
https://github.com/evancz/elm-http/blob/3.0.1/src/Http.elm#L56
-}
queryParamsToUrl : String -> List ( String, String ) -> String
queryParamsToUrl baseUrl args =
    let
        queryPair : ( String, String ) -> String
        queryPair ( key, value ) =
            queryEscape key ++ "=" ++ queryEscape value

        queryEscape : String -> String
        queryEscape string =
            String.join "+" (String.split "%20" (Http.encodeUri string))
    in
        case args of
            [] ->
                baseUrl

            _ ->
                baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


{-| Copy pasta of Json.Decode.Extra.apply
-}
apply : Decode.Decoder (a -> b) -> Decode.Decoder a -> Decode.Decoder b
apply =
    Decode.map2 (<|)
