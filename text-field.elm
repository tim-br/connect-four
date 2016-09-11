import Html exposing (Html, Attribute, div, input, text, button, span)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Random


main =
  App.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { content : Int
  , guess : Int
  , targetnumber : Int
  , won : Bool
  , seed : Random.Seed
  , amountToBet : Int
  }

-- goal text
winner : String
winner = "text to"

seed0 = Random.initialSeed 31415

bar : Random.Generator Int
bar = Random.int 0 100

foo = Random.step (Random.int 0 1) seed0  --Random.generate seed0 bar  -- ==> (42, seed1)

amount = 100

--randomNumber = Random.generate (Random.int 0 8) seed0

model : Model
model =
  { content = amount
  , guess = 0
  , targetnumber = 0
  , seed = seed0
  , won = True
  , amountToBet = 0 }


-- UPDATE

type Msg
  = Change String | Bet | GenRan | Guess String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | amountToBet = String.toInt newContent |> Result.toMaybe |> Maybe.withDefault 0 }
    Guess guess ->
      { model |  guess = String.toInt guess |> Result.toMaybe |> Maybe.withDefault 0 }
    Bet ->
      let (res , newseed) = Random.step (Random.int 0 1) model.seed
          _ = Debug.log "the res is " res
      in
      if model.guess == res then
      { model | content = model.content + model.amountToBet, seed = newseed}
      else
      { model | content = model.content - model.amountToBet, seed = newseed}
    GenRan ->
      let (res , newseed) = Random.step (Random.int 0 10) model.seed
      in
      {model | seed = newseed, targetnumber = res}



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "amount to bet", onInput Change ] []
    , input [ placeholder "your guess", onInput Guess ] []
    , div [] [text "then amount to bet and the guess is "]
    , div [] [text (toString model.amountToBet)]
    , div [] [text (toString model.guess)]
    , div [] [ text (toString model.content) ]
    , span [] [ text "the target num is " ]
    , span [] [ text (toString model.targetnumber) ]
    , div [] [text (toString foo)]
    --, div [] [ text (toString (Random.generate (Random.int 0 8) seed0)) ]
    , button [Html.Events.onClick Bet] [text "bet"]
    , button [Html.Events.onClick GenRan] [text "get a random number"]
    , div [] [if model.won == True then text ("You have won") else text ""]
    ]
