module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


greet : String -> String
greet inp =
    "Hello, " ++ inp


type CardState
    = Open
    | Closed
    | Matched


type CardGroup
    = A
    | B


type alias Card =
    { id : String, state : CardState, group : CardGroup }


type alias Deck =
    List Card


type alias Model =
    { cards : Deck
    }


init : Model
init =
    { cards =
        [ { id = "1"
          , state = Closed
          , group = A
          }
        , { id = "2"
          , state = Closed
          , group = A
          }
        , { id = "3"
          , state = Closed
          , group = A
          }
        , { id = "1"
          , state = Closed
          , group = B
          }
        , { id = "2"
          , state = Closed
          , group = B
          }
        , { id = "3"
          , state = Closed
          , group = B
          }
        ]
    }


type Msg
    = CardClicked Card


getCardClassAndPathAndClick : Card -> ( List (Attribute Msg), String, String )
getCardClassAndPathAndClick c =
    case c.state of
        Open ->
            ( [], "open", "/static/cats/" ++ c.id ++ ".jpg" )

        Closed ->
            ( [ onClick (CardClicked c) ], "closed", "/static/cats/closed.png" )

        Matched ->
            ( [], "matched", "/static/cats/" ++ c.id ++ ".jpg" )


viewcard : Card -> Html Msg
viewcard c =
    let
        ( attrs, cl, path ) =
            getCardClassAndPathAndClick c
    in
        div attrs
            [ img [ class cl, src path ] []
            ]


viewCards : Model -> Html Msg
viewCards model =
    model.cards |> List.map viewcard |> div [ class "cards" ]


view : Model -> Html Msg
view m =
    viewCards m


setCard : CardState -> Maybe Card -> List Card -> List Card
setCard newState aCard deck =
    case aCard of
        Nothing ->
            deck

        Just card ->
            deck
                |> List.map
                    (\elm ->
                        if elm.id == card.id && elm.group == card.group then
                            { elm | state = newState }
                        else
                            elm
                    )


type alias FilterCardFun =
    Card -> Card -> Bool


getAnotherOpenCard : FilterCardFun -> Card -> Deck -> Maybe Card
getAnotherOpenCard fltFun card deck =
    deck
        |> List.filter (fltFun card)
        |> List.head



{- }




   unmatchingIsOpen : Card -> Deck -> Bool
   unmatchingIsOpen c deck =
       deck
           |> List.filter (\elm -> elm.id /= c.id && elm.state == Open)
           |> List.head

-}
{-
      Only 1 Open card is allowed!
      It's either matching or not
         when matching - set both to matched, in other words - both with this Id
         to Matched

         when unmatching - set both to closed
             in other words, return List.Map with Closed for this and the other



   workWithCard : Card -> Deck -> Card -> Card
   workWithCard c d card =
       case card.state of
           Matched ->
               card

           Open ->
               { card | state = Closed }

           Closed ->
               if aMatchingCardIsOpen card d c then
                   setCard Matched card
               else
                   card

-}


matchingFilter : Card -> Card -> Bool
matchingFilter c elm =
    elm.id == c.id && elm.group /= c.group && elm.state == Open


type IsMatch
    = Yes Deck
    | No Deck


aMatchingCardIsOpen : Card -> Deck -> Deck
aMatchingCardIsOpen c deck =
    if deck |> List.any (\elm -> elm.id == c.id && elm.group /= c.group && elm.state == Open) then
        deck
            |> setCard Matched
                (getAnotherOpenCard
                    matchingFilter
                    c
                    deck
                )
            |> setCard Matched (Just c)
    else
        deck


update : Msg -> Model -> Model
update msg model =
    case msg of
        CardClicked c ->
            { model
                | cards =
                    model.cards
                        |> aMatchingCardIsOpen c
                        |> setCard Open (Just c)
            }


main =
    Html.beginnerProgram { model = init, view = view, update = update }



--    viewCards init
