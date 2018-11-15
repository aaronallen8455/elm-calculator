import Browser
import Html exposing (Html, div, table, tr, td, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { display : String            -- What gets displayed by the UI
  , productAccum : Maybe Float  -- If a product operation (*,/) is in progress, it is accumulated here
  , accumulator : Float         -- Accumulates the overall result
  , prevOperator : Maybe Op     -- Holds the previous operation, if one exists
  , lastKeyWasOperator : Bool   -- True if the last key pressed was an operator key
  }

-- | The initial state
init : Model
init =
  { display = "0"
  , productAccum = Nothing
  , accumulator = 0
  , prevOperator = Nothing
  , lastKeyWasOperator = True }


-- UPDATE

type Op = Product ProductOp | Sum SumOp
type SumOp = Add | Subtract
type ProductOp = Multiply | Divide

type Msg = Num String | Operator Op | Equals

-- | Updates the Model given a message from the UI
update : Msg -> Model -> Model
update msg model = 
  -- read the number out of the display
  let displayNum = Maybe.withDefault 0 ( String.toFloat ( .display model ) ) 
  in case msg of
    -- A numeric key was pressed
    Num key -> numKeyPressed key model
    -- An operator key was pressed
    Operator op -> operatorKeyPressed op displayNum model
    -- equals key was pressed
    Equals -> equalsKeyPressed displayNum model

-- | When a number key (or the decimal) is pressed, update the display value
numKeyPressed : String -> Model -> Model
numKeyPressed key model = case key of
  -- add a decimal point or reset if there already was one
  "." -> if String.contains "." ( .display model )
          then { model | display = "0.", prevOperator = Nothing, lastKeyWasOperator = False }
          else { model | display = ( .display model ) ++ key, lastKeyWasOperator = False }
  -- append the number to the end of the display string if the last key was not an operator, otherwise start new string
  _ -> let newDisplay = if .lastKeyWasOperator model then key else .display model ++ key
        in { model | display = newDisplay, lastKeyWasOperator = False }
           |> checkOverflow

-- | When an operator key is pressed, update the internal state
-- and display an intermediate result if determinable based upon
-- the operator pressed and the previous operator, if one exists.
operatorKeyPressed : Op -> Float -> Model -> Model
operatorKeyPressed op displayNum model =
  case op of
    -- current operation is a sum
    Sum _ -> case .prevOperator model of
      -- prev operation was a sum
      Just ( Sum prevSumOp ) ->
        -- negate the display number if prev operation was subtraction
        let displayNum_ = displayNum * ( if prevSumOp == Add then 1 else -1 )
            newAccumulator = ( .accumulator model ) + displayNum_
        in { model | display = String.fromFloat newAccumulator
                   , accumulator = newAccumulator
                   , productAccum = Nothing
                   , prevOperator = Just op
                   , lastKeyWasOperator = True }
           |> checkOverflow 
      -- prev operation was a product
      Just ( Product prevProdOp ) ->
        -- invert the display number if the prev operation was division
        let displayNum_ = if prevProdOp == Multiply then displayNum else 1 / displayNum
            product = Maybe.withDefault 0 ( .productAccum model ) * displayNum_
            newAccumulator = .accumulator model + product
        in  { model | display = String.fromFloat newAccumulator
                    , accumulator = newAccumulator
                    , productAccum = Nothing
                    , prevOperator = Just op
                    , lastKeyWasOperator = True }
            |> checkOverflow
            |> checkDivision prevProdOp displayNum

      -- no previous operation
      Nothing -> { model | prevOperator = Just op
                         , lastKeyWasOperator = True
                         , accumulator = displayNum }

    -- current operation is a product
    Product _ -> case .prevOperator model of
      -- prev operation was a sum
      Just ( Sum prevSumOp ) ->
        let newProdAccum = displayNum * ( if prevSumOp == Add then 1 else -1 )
        in { model | productAccum = Just newProdAccum
                   , prevOperator = Just op
                   , lastKeyWasOperator = True }
      -- prev operation was a product
      Just ( Product prevProdOp) ->
        let displayNum_ = if prevProdOp == Multiply then displayNum else 1 / displayNum
            newProductAccum = ( Maybe.withDefault 1 ( .productAccum model ) ) * displayNum_
            newDisplay = if .accumulator model == 0 then String.fromFloat newProductAccum else .display model
        in { model | display = newDisplay
                   , productAccum = Just newProductAccum
                   , prevOperator = Just op
                   , lastKeyWasOperator = True }
           |> checkOverflow
           |> checkDivision prevProdOp displayNum

      -- no previous operation
      Nothing -> { model | prevOperator = Just op
                         , lastKeyWasOperator = True 
                         , productAccum = Just displayNum }

-- | When the equals key is pressed, display the final result and reset the internal state.
equalsKeyPressed : Float -> Model -> Model
equalsKeyPressed displayNum model =
  case .prevOperator model of
    -- final operation is a sum
    Just ( Sum sumOp ) ->
      let displayNum_ = if sumOp == Add then displayNum else displayNum * -1
          newAccumulator = .accumulator model 
                              + displayNum_
                              + Maybe.withDefault 0 ( .productAccum model )
      in { init | display = String.fromFloat newAccumulator }
         |> checkOverflow

    -- final operation is a product
    Just ( Product prodOp ) ->
      let displayNum_ = if prodOp == Multiply then displayNum else 1 / displayNum
          product = Maybe.withDefault 0 ( .productAccum model ) * displayNum_
          newAccumulator = .accumulator model + product
      in { init | display = String.fromFloat newAccumulator }
         |> checkOverflow
         |> checkDivision prodOp displayNum

    -- reset if no operation
    Nothing -> init

-- | Check if the display string is longer than 10 digits
-- if so, display Error and reset.
checkOverflow : Model -> Model
checkOverflow model =
  if String.length ( ( String.filter (\c -> c /= '.') ) ( .display model ) ) > 10
  then { init | display = "Error" }
  else model

-- | Checks if trying to divide something by 0. 
-- Changes display to Undefined and resets if so.
checkDivision : ProductOp -> Float -> Model -> Model
checkDivision op divisor model = case op of
  Divide ->
    if divisor == 0
    then { init | display = "Undefined" } 
    else model
  Multiply -> model


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "calculator" ]
    [ div [ class "display-wrapper" ]
        [ div [ id "display" ] [ text ( .display model ) ] ]
    , table [ class "keypad" ]
      [ tr []
          [ td [ onClick ( Num "7" ) ] [ text "7" ] 
          , td [ onClick ( Num "8" ) ] [ text "8" ]
          , td [ onClick ( Num "9" ) ] [ text "9" ]
          , td [ onClick ( Operator ( Product Divide ) ) ] [ text "÷" ] 
          ]
      , tr []
          [ td [ onClick ( Num "4" ) ] [ text "4" ] 
          , td [ onClick ( Num "5" ) ] [ text "5" ]
          , td [ onClick ( Num "6" ) ] [ text "6" ]
          , td [ onClick ( Operator ( Product Multiply ) ) ] [ text "×" ] 
          ]
      , tr []
          [ td [ onClick ( Num "1" ) ] [ text "1" ] 
          , td [ onClick ( Num "2" ) ] [ text "2" ]
          , td [ onClick ( Num "3" ) ] [ text "3" ]
          , td [ onClick ( Operator ( Sum Subtract ) ) ] [ text "-" ] 
          ]
      , tr []
          [ td [ onClick ( Num "0" ) ] [ text "0" ] 
          , td [ onClick ( Num "." ) ] [ text "." ]
          , td [ onClick ( Operator ( Sum Add) ) ] [ text "+" ]
          , td [ onClick Equals ] [ text "=" ] 
          ]
      ]
    ]
  