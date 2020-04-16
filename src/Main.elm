module Main exposing (..)


import Browser
import Browser.Events
import Tuple
import Time
import Random
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Table as Table
import Bootstrap.Progress as Progress
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Grid.Col as Col


-- This pages main

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

coef : () -> List Float
coef _ =  [ -6.186929
          , 0.380431
          , 0.09
          , 4.94659
          , -0.793333
          , 0.0323484
          , 0.000111
          , -0.236667
          , -0.010402
          , -0.002299
          ]

-- Model


type alias Operator = { name : String
                      , mean : Float
                      , sd : Float
                      }


type alias Operators = List Operator

initOperators = [ { name = "Alice", mean = 0.05, sd = 0.05}
                , { name = "Bob", mean = 0.10, sd = 0.02}
                , { name = "Carol", mean = -0.10, sd = 0.02}
                , { name = "Doug", mean = -0.05, sd = 0.05}
                ]


type alias Config = { trialTimeOffset : Int 
                    , debug : Bool
                    }


initConfig = { trialTimeOffset = 0 , debug = False}


type Factor = Pressure | Time | Vibration


type Level = Minus | Plus


type alias FactorLevel = { factor : Factor, level : Level}


type alias Opts = { pressure : List Float
                  , time : List Float
                  , vibration : List Float
                  }


type alias TrialValues =    { pressure : Float
                            , time : Float
                            , vibration : Float
                            }


fRange : Float -> Int -> Float -> List Float
fRange start n step =
    List.range 0 n
    |> List.map toFloat
    |> List.map (\x -> start + step*x)


roundFloat : Int -> Float -> Float
roundFloat digits n =
   let
     div = toFloat 10^(toFloat digits)
     shifted = n*div
   in
     round shifted |> toFloat |> \x -> x/div


initOpts = { pressure = fRange 2 20 0.1
           , time = fRange 2 10 0.1
           , vibration = fRange 28 30 1
           }

type alias DropdownStateRow = { minus : Dropdown.State, plus : Dropdown.State }

initDropdownStateRow = { minus = Dropdown.initialState, plus = Dropdown.initialState }

type alias DropdownState = { pressure : DropdownStateRow
                           , time : DropdownStateRow
                           , vibration : DropdownStateRow
                           }

initDropState = { pressure = initDropdownStateRow
                , time = initDropdownStateRow
                , vibration = initDropdownStateRow
                }

type alias TrialsArePlus =  { pressure : Bool
                          , time : Bool
                          , vibration : Bool
                          }

initTrialArePlus =  { pressure = True
                   , time = True
                   , vibration = True
                   }

type alias Model =
    { dropState : DropdownState
    , pressure : (Float, Float)
    , time : (Float, Float) 
    , vibration : (Float, Float)
    , sd : Float
    , trials :  TrialsArePlus
    , result : Maybe Float
    , ticks : Maybe Int
    , opName : String
    , opMean : Float
    , opSD : Float
    , opDropState : Dropdown.State
    }


initModel ={ dropState = initDropState
           , pressure = (2, 4)
           , time = (2, 3)
           , vibration = (28, 57)
           , sd = 0.122
           , trials = initTrialArePlus
           , result = Nothing
           , ticks = Nothing
           , opName = "Alice"
           , opMean = 0.05
           , opSD = 0.05
           , opDropState = Dropdown.initialState
           } 


getDropStateRow : Factor -> Model -> DropdownStateRow
getDropStateRow factor model =
    let
        dropState = model.dropState
    in
        case factor of
        Pressure -> dropState.pressure
        Time -> dropState.time
        Vibration -> dropState.vibration

setDropStateRow : Level -> Dropdown.State -> DropdownStateRow -> DropdownStateRow
setDropStateRow level state rowState =
    case level of
       Minus ->
            {rowState | minus = state }

       Plus ->
            {rowState | plus = state }


getDropState : Factor -> Level -> Model-> Dropdown.State
getDropState factor level model =
    let
        rowState = model |> getDropStateRow factor
    in
        case level of
            Minus -> rowState.minus
            Plus -> rowState.plus



setDropState : Factor -> Level -> Dropdown.State -> Model -> Model
setDropState factor level state model =
    let
        rowState = getDropStateRow factor model
        newRowState = setDropStateRow level state rowState
        dropState = model.dropState
        newDropState =
            case factor of
               Pressure ->
                  { dropState | pressure = newRowState}
               Time ->
                  { dropState | time = newRowState}
               Vibration ->
                  { dropState | vibration = newRowState}
    in
        { model | dropState = newDropState}


-- Dropdowns depends on view state to keep track of whether it is (/should be) open or not


getOpts : Factor -> List Float
getOpts factor =
    case factor of
        Pressure -> initOpts.pressure
        Time -> initOpts.time
        Vibration -> initOpts.vibration
     

getLevels : Factor -> Model -> (Float, Float)
getLevels factor model =
    case factor of
        Pressure -> model.pressure
        Time -> model.time
        Vibration -> model.vibration

getFactorLevel : Factor -> Level -> Model -> Float
getFactorLevel factor level model =
    let
        getVal = 
            case level of
               Minus -> Tuple.first
               Plus -> Tuple.second
    in
        model 
        |> getLevels factor   
        |> getVal


setFactorLevel : Factor -> Level -> Float -> Model -> Model
setFactorLevel factor level value model =
    let
        (f, s) = model |> getLevels factor
        newLevels = 
            case level of
                Minus ->
                    if value < s then (value, s) else (f, s)
                Plus ->
                    if value > f then (f, value) else (f, s)
    in
        case factor of
           Pressure ->
                {model | pressure = newLevels}

           Time ->
                {model | time = newLevels}

           Vibration ->
                {model | vibration = newLevels}


setOperator : String -> Float -> Float -> Model -> Model
setOperator name mean sd model =
    { model | opName = name, opMean = mean, opSD = sd}
            

getTrial : Factor -> Model -> Bool
getTrial factor model =
    let
        trials = model.trials
    in
        case factor of
            Pressure -> trials.pressure
            Time -> trials.time
            Vibration -> trials.vibration

toggleTrial : Factor -> Model -> Model
toggleTrial factor model =
    let
        trials = model.trials
        newTrials =
            case factor of
               Pressure -> { trials | pressure = not trials.pressure}
               Time -> { trials | time = not trials.time}
               Vibration -> { trials | vibration = not trials.vibration}
    in
        { model | trials = newTrials}
    
getTrialFactorLevel : Factor -> Model -> Float
getTrialFactorLevel factor model =
    let
        isPlus = getTrial factor model
        (m, p) = getLevels factor model
    in
        if isPlus then p else m

getTrialFactors : Model -> TrialValues
getTrialFactors model =
    { pressure = getTrialFactorLevel Pressure model
    , time = getTrialFactorLevel Time model
    , vibration = getTrialFactorLevel Vibration model
    }

fittedValue model =
    let
        values = model |> getTrialFactors
        p = values.pressure
        t = values.time
        v = values.vibration
        vars =  [ 1
                , p
                , p*p
                , t
                , t*t
                , v
                , v*v
                , p*t
                , p*v
                , t*v
                ]
    in
        List.map2 (*) (coef ()) vars
        |> List.sum
    

observedValue : Float ->  Model -> Float
observedValue z model =
    (fittedValue model) + model.sd * z


-- init
init : () -> (Model, Cmd Msg)
init _ = 
    (  initModel
    , Cmd.none
    )


-- Msg

type Msg
    = MyDropMsg Factor Level Dropdown.State
    | OpDropMsg Dropdown.State
    | ChangeLevel Factor Level Float
    | ChangeOperator String Float Float
    | ToggleTrial Factor
    | RunTrial
    | NewRandomPair (Float, Float)
    | Tick Time.Posix


-- In your update function you will to handle messages coming from the dropdown

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MyDropMsg factor level state ->
            ( model |> setDropState factor level state
            , Cmd.none
            )

        OpDropMsg state ->
            ( {model | opDropState = state}
            , Cmd.none
            )

        ChangeLevel factor level value ->
            ( model |> setFactorLevel factor level value
            , Cmd.none
            )

        ChangeOperator name mean sd ->
            ( model |> setOperator name mean sd
            , Cmd.none
            )

        RunTrial ->
            (model
            , getRandomPair model
            )


        ToggleTrial factor ->
            ( model |> toggleTrial factor 
            , Cmd.none
            )

        NewRandomPair (u1, u2) ->
            let
                (z1, z2) = normalTransform u1 u2
                obs = model |> observedValue z1
                operator = model.opMean + model.opSD * z2
            in
                ( { model | result = Just (obs + operator) } |> startTicks 
                , Cmd.none
                )

        Tick _ ->
            let
                nextTick = 
                    case model.ticks of
                        Nothing ->
                            Nothing
                        
                        Just x ->
                            if x == 1 then Nothing else Just (x - 1)
            in
                ({ model | ticks = nextTick }
                , Cmd.none
                )


totalTicks model =
    let
        values = model |> getTrialFactors
        t = values.time
        offset = initConfig.trialTimeOffset  
    in
        offset + t |> (*) 60 |> round


startTicks model =
    let
        ticks = totalTicks model
    in
        { model | ticks = Just ticks }
    



a u1 =
    u1
    |> logBase e
    |> (*) -2
    |> sqrt


b1 u2 = 
    u2
    --|> turns
    |> (*) (2*pi)
    |> sin


b2 u2 = 
    u2
    --|> turns
    |> (*) (2*pi)
    |> cos


normalTransform : Float -> Float -> (Float, Float)
normalTransform u1 u2 =
    let
        z1 = a(u1)*b1(u2)
        z2 = a(u1)*b2(u2)
    in
        (z1, z2)


getRandomPair :  Model -> Cmd Msg
getRandomPair model =
    Random.generate NewRandomPair (Random.pair (Random.float 0 1) (Random.float 0 1))


-- Dropdowns relies on subscriptions to automatically close any open when clicking outside them

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pressureState = model |> getDropStateRow Pressure
        timeState = model |> getDropStateRow Time
        vibrationState = model |> getDropStateRow Vibration
    in
        case model.ticks of
            Nothing ->
                Sub.batch
                    [ Dropdown.subscriptions (model |> getDropState Pressure Minus) (MyDropMsg Pressure Minus)
                    , Dropdown.subscriptions (model |> getDropState Pressure Plus) (MyDropMsg Pressure Plus)
                    , Dropdown.subscriptions (model |> getDropState Time Minus) (MyDropMsg Time Minus)
                    , Dropdown.subscriptions (model |> getDropState Time Plus) (MyDropMsg Time Plus)
                    , Dropdown.subscriptions (model |> getDropState Vibration Minus) (MyDropMsg Vibration Minus)
                    , Dropdown.subscriptions (model |> getDropState Vibration Plus) (MyDropMsg Vibration Plus)
                    , Dropdown.subscriptions model.opDropState OpDropMsg
                    ]
            
            Just tick ->
                Sub.batch
                    [ Browser.Events.onAnimationFrame Tick ]


-- Specify config and how the dropdown should look in your view (or view helper) function


-- view helpers

dataString : Factor  -> Float -> String
dataString factor val =
    let
        fStr = val 
            |> roundFloat 2
            |> String.fromFloat
        
        units  = 
            case factor of
                Pressure -> " bar"
                Time -> " sec"
                Vibration -> " µm"
    in
        fStr ++ units
    


dropDownButton : Factor -> Level -> Float -> Dropdown.DropdownItem Msg
dropDownButton factor level val =
    Dropdown.buttonItem [ onClick (ChangeLevel factor level val)]   [ text (dataString factor val)]


dropDownButtonOp : Operator -> Dropdown.DropdownItem Msg
dropDownButtonOp operator =
    let
        name = operator.name
        mean = operator.mean
        sd = operator.sd
    in
        Dropdown.buttonItem [ onClick (ChangeOperator name mean sd)]   [ text name ]


entryDropdown : Factor -> Level -> Model -> Html Msg
entryDropdown factor level model =
    let
        opts = getOpts factor
    in
        div []
            [ Dropdown.dropdown
                (model |> getDropState factor level)
                { options = [ ]
                , toggleMsg = (MyDropMsg factor level)
                , toggleButton =
                    Dropdown.toggle [ Button.outlinePrimary, Button.small ] 
                                    [ model
                                        |> getFactorLevel factor level
                                        |> dataString factor
                                        |> text
                                    ]
                , items = opts |> List.map (dropDownButton factor level) 
                }

            -- etc
            ]

entryDropdownOp : Operators -> Model -> Html Msg
entryDropdownOp operators model =
    div []
        [ Dropdown.dropdown model.opDropState
            { options = [ ]
            , toggleMsg = OpDropMsg
            , toggleButton =
                Dropdown.toggle [ Button.success, Button.small ] 
                                [ model.opName
                                    |> text
                                ]
            , items = operators |> List.map dropDownButtonOp 
            }
        ]

debug : Config -> Model -> List (Html Msg)
debug config model =
    if config.debug then 
        [ model |> Debug.toString |> text ] 
    else
        []


mainGrid tableView trialsView debugView getResultButton resultView = 
    div [] [ Grid.container []
                [CDN.stylesheet -- creates an inline style node 
                , Grid.row []
                    [ Grid.col  [ Col.md12] 
                                [ h2 [] [ Html.text "Weld Strength Experiment"]
                                , Html.a [ href "https://en.wikipedia.org/wiki/Ultrasonic_welding#/media/File:Ultrasonic_Welding.JPG"] 
                                    [ img [src "img/Ultrasonic_Welding.JPG"] []]
                                ]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md8] 
                                [ h3 [] [ Html.text "Factors"]]
                    , Grid.col  [ Col.md4]
                                [ h3 [] [ Html.text "Execute a Trial"]]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md8] 
                                [ tableView ]
                    , Grid.col [ Col.md4]
                               [ trialsView ]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md8] 
                                []
                    , Grid.col [ Col.md4] 
                                [ getResultButton]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md8] 
                                []
                    , Grid.col [ Col.md4] 
                                [ resultView ]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md8] 
                                []
                    , Grid.col [ Col.md4] 
                                debugView
                    ]
                ]
            ]


factorTable : Model -> Html Msg
factorTable model =
            div []
                [ Table.table
                    { options = [ Table.striped
                                , Table.hover 
                                , Table.bordered
                                , Table.small
                                ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ text "ID" ]
                        , Table.th [] [ text "Name" ]
                        , Table.th [] [ text "-" ]
                        , Table.th [] [ text "+" ]
                        ]
                    , tbody =
                        Table.tbody []
                            [ Table.tr []
                                [ Table.td [] [ text "A" ]
                                , Table.td [] [ text "Weld pressure (bar)" ]
                                , Table.td [] [ entryDropdown Pressure Minus model ]
                                , Table.td [] [ entryDropdown Pressure Plus model ]
                                ]
                            , Table.tr []
                                [ Table.td [] [ text "B" ]
                                , Table.td [] [ text "Weld time (sec)" ]
                                , Table.td [] [ entryDropdown Time Minus model ]
                                , Table.td [] [ entryDropdown Time Plus model ]
                                ]
                            , Table.tr []
                                [ Table.td [] [ text "C" ]
                                , Table.td [] [ text "Vibration (µm)" ]
                                , Table.td [] [ entryDropdown Vibration Minus model ]
                                , Table.td [] [ entryDropdown Vibration Plus model ]
                                ]
                            ]
                    }
                ]

trialLevelView : Factor -> Model -> String
trialLevelView factor model =
    let
        isPlus = getTrial factor model
        (m, p) = getLevels factor model
    in
        if isPlus then "+" else "-"


result model = 
    let
        out = 
            case model.result of
                Nothing -> 
                    ""
                Just y -> 
                    y 
                    |> roundFloat 4 
                    |> String.fromFloat 
                    |> \s -> "y = " ++ s ++ " N/mm\u{00B2}" 
    in
        case model.ticks of
            Nothing ->
                out |> text
            Just t ->
                let
                    tot = model |> totalTicks |> toFloat
                    num = t |> toFloat
                    percent = 100 * num / tot 
                in
                    Progress.progress [ Progress.success, Progress.value ((5/3)*(100 - percent))]




toggleButton factor model =
    Button.button
        [ Button.small
        , Button.success
        , Button.onClick (ToggleTrial factor)
        ]
        [ b [] [model |> trialLevelView factor |> text ]]
    


resultsButtonText model =
    case model.ticks of
        Nothing ->
            [b [] [ text "Run Trial"]] 

        Just _ ->
            [ Spinner.spinner
                [ Spinner.small, Spinner.attrs [ Spacing.mr1 ] ] []
            , text "Welding"
            ]


getResults model =
        Button.button
                [ Button.small
                , Button.primary
                , Button.onClick RunTrial
                ]
                (resultsButtonText model)


trialTable : Model -> Html Msg
trialTable model =
            div []
                [ Table.table
                    { options = [ Table.striped
                                , Table.hover 
                                , Table.bordered
                                , Table.small
                                ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ text "A" ]
                        , Table.th [] [ text "B" ]
                        , Table.th [] [ text "C" ]
                        , Table.th [] [ text "Operator" ]
                        ]
                    , tbody =
                        Table.tbody []
                            [ Table.tr []
                                [ Table.td [] [ toggleButton Pressure model ] 
                                , Table.td [] [ toggleButton Time model ] 
                                , Table.td [] [ toggleButton Vibration model ] 
                                , Table.td [] [ entryDropdownOp initOperators model] 
                                ]
                            ]
                    }
                ]
-- main view for subpage debug


view : Model -> Html Msg
view model =
    mainGrid (factorTable model) (trialTable model) (debug initConfig model) (getResults model) (result model)
