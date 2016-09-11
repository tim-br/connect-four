import Html exposing (Html, button, div, text, header, footer)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array


main =
  App.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type XORY = X | Y
type Turn = XPlayer | YPlayer

type alias TD = List (List XORY)

car : TD
car = []

type alias Model =
  {list: TD
  , over: Bool
  , turn: Turn
  }

model : Model
model =
  {list = car --[[Y,X] , [X, Y]]
  , over = False
  , turn = XPlayer
  }


-- UPDATE

checkElems : List XORY -> Bool
checkElems mList = False

-- tailIfEmpty : List a -> List a
-- tailIfEmpty mList =
--   case List.isEmpty mList of
--     True -> []
--     False -> List.tail mList

equal : XORY -> XORY -> Bool
equal a b =
  case a == b of
    True -> True
    False -> False

allEqual : List XORY -> XORY
allEqual mList = List.foldl (\y x -> (if (equal x y) then x else y)) X mList

first2Equal : List XORY -> Bool
first2Equal xli =
    case xli of
        a::b::_ ->
          a == b
        _ ->
          False

last3ElemsHelper : (List a) -> Maybe (List a)
last3ElemsHelper x =
  case x of
    a::b::c::[] -> Just [a, b, c]
    [] -> Nothing
    a::[] -> Nothing
    a::b::[] -> Nothing
    a::b -> last3ElemsHelper b

last mList = List.foldl (\y x -> x) [] mList

-- isWon : Model -> Model
-- isWon model =
--   {model | over = first2Equal model.list}

type Msg = AppendY | AppendX | AppendYRow2 | AppendXRow2 | Reset

appendY : (List XORY) -> (List XORY)
appendY li = List.append li [Y]

appendX : (List XORY) -> (List XORY)
appendX li = List.append li [X]

headTD : TD -> (List XORY)
headTD td = td
            |> List.head
            |> getValueFromTD

tailTD : TD -> (List XORY)
tailTD td =  (List.drop 1 td)
             |> List.head
             |> getValueFromTD

update : Msg -> Model -> Model
update msg model =
  if model.over == False then
    case msg of
      AppendY ->
        case model.turn of
          YPlayer ->
            let newHead = model.list
                          |> headTD
                          |> appendY
                li = newHead :: [(tailTD model.list)] --List.append model.list [Y]
                ov = last3ElementsEqual newHead
            in
            { model | list = li, over = ov, turn = XPlayer}
          XPlayer ->
            model

      AppendX ->
        case model.turn of
          XPlayer ->
            let newHead = model.list
                          |> headTD
                          |> appendX
                li = newHead :: [(tailTD model.list)]  --List.append model.list [X]
                ov = last3ElementsEqual newHead
            in
            { model | list = li, over = ov, turn = YPlayer}
          YPlayer ->
            model

      AppendYRow2 ->
        case model.turn of
        YPlayer ->
          let newTail = model.list
                        |> tailTD
                        |> appendY
              li = (headTD model.list) :: [newTail] --model.list  --List.append model.list [X]
              ov = last3ElementsEqual newTail
          in
          { model | list = li, over = ov, turn = XPlayer}
        XPlayer ->
          model
      AppendXRow2 ->
        case model.turn of
        XPlayer ->
          let newTail = model.list
                        |> tailTD
                        |> appendX
              li = (headTD model.list) :: [newTail] --model.list  --List.append model.list [X]
              ov = last3ElementsEqual newTail
        in
        { model | list = li, over = ov, turn = YPlayer}
        YPlayer ->
          model
      Reset ->
        { model | list = [], over = False }
  else
    if msg == Reset then
      { model | list = [], over = False }
    else
      model

-- VIEW

li = [5, 5, 5, 6]
q = last3ElemsHelper li

foo : TD
foo = [[X, Y, X, Y],  [Y, X, Y]]

getValueFromTD : Maybe (List XORY) -> (List XORY)
getValueFromTD x =
  case x of
    Just foo -> foo
    Nothing -> []

last3ElementsEqual li =
  let elems = last3ElemsHelper li
  in
  case elems of
    Nothing -> False
    Just li ->
      let new = Array.fromList li
          first = Array.get 0 new
          sec = Array.get 1 new
          third = Array.get 2 new
      in
      if first == sec then sec == third
        else False

view : Model -> Html Msg
view model =
  div []
  [ button [ onClick AppendX ] [ text "Append X" ]
  , div [] [ text (toString model) ]
  , button [ onClick AppendY ] [ text "Append Y" ]
  , button [ onClick AppendYRow2 ] [ text "Append Y row 2" ]
  , button [ onClick AppendXRow2 ] [ text "Append X row 2" ]
  , button [ onClick Reset ] [ text "Reset" ]
  ]

-- div [] [text (toString ( [X, Y] :: [(tailTD model.list)]))] debugging stuff

  -- div [id "contenido-principal"]
  --   [
  --     header [] [text "Soy el head"],
  --     div [id "contenido"]
  --     [text "Contenido"],
  --     footer [] [text "Soy el pie"]
  --   ]
