module Main exposing (..)


import Browser 
import Html exposing (Html, button, div, text, ol, li)
import Html.Events exposing (onClick)
import Dict exposing (update)
import List exposing (map)
import Html.Attributes exposing (value)



main = 
    Browser.sandbox {init = init, update=update ,view=view}



-- Types
type alias ComprehensiveRecord  =
    { penaltyAmount : Float
    , email : String
    , name: String
    }

type alias BasicRecord = 
    {
        name: String
    ,   email : Maybe String
    }


type PerUserData 
    = ComprehensiveRecordData ComprehensiveRecord
    | BasicData BasicRecord 


-- Lens and prisms
-- This should happen one layer at a time to stop unnessary code from being writtern !!!!!!!!!!

type alias Lens structure value = 
    {
        get : structure -> value 
    ,   set : structure -> value -> structure 
    }

type alias Prism structure value = 
    {   get: structure -> Maybe value
    ,   set: structure -> value -> structure 
    }


-- Lens and prism implementations


getPerUserEmail: Prism PerUserData String 
getPerUserEmail  = 
    {
        get = \structure ->
            case structure of
                BasicData a -> a.email
                ComprehensiveRecordData v -> Just v.email
    ,   set =  \structure value ->
            case structure of 
                ComprehensiveRecordData v -> ComprehensiveRecordData {v | email = value}
                BasicData v -> 
                    case v.email of 
                        Nothing -> BasicData v
                        Just _ -> BasicData {v | email = Just value}
    }

getBasicEmail: Prism BasicRecord String 
getBasicEmail = 
    {
        get = \structure -> 
            case structure.email of 
                Nothing -> Nothing
                Just a -> Just a
    ,   set = \structure value ->
            case structure.email of
                Nothing -> structure
                Just _ -> {structure | email= Just value}
    }


getComprehensiveEmail: Lens ComprehensiveRecord String 
getComprehensiveEmail =
    {
        get = \structure -> structure.email
    ,   set = \structure value -> {structure | email = value}
    }



getComprehensivePenalty: Lens ComprehensiveRecord Float 
getComprehensivePenalty =
    {
        get = \structure -> structure.penaltyAmount
    ,   set = \structure value -> {structure | penaltyAmount = value}
    }


getPerUserName: Lens PerUserData String
getPerUserName = 
    {
        get =  \structure -> 
            case structure of 
                ComprehensiveRecordData v -> v.name 
                BasicData v -> v.name
    ,   set = \structure value -> 
            case structure of 
                ComprehensiveRecordData v -> ComprehensiveRecordData {v | name = value} 
                BasicData v -> BasicData {v | name = value}
    }



getComprehensiveRecord: PerUserData -> Maybe ComprehensiveRecord
getComprehensiveRecord userData 
    = case userData of
        BasicData _ -> Nothing
        ComprehensiveRecordData v -> Just v



randomName = BasicData {
        name = "carl"
    ,   email = Just "Hello"
    }



-- i want to get PerUserData which is a prism 
-- then a lens for name 

getUserData: Prism Model PerUserData
getUserData = 
    {
        get = \structure -> 
            case structure.perUserData of
                    Nothing -> Nothing
                    Just a -> Just a 
    ,    set = \structure value ->
                case structure.perUserData of 
                    Nothing -> structure
                    Just _ -> {structure | perUserData = Just value} 
    }



-- Compose 
-- Model to string
-- prism and Lens compose 
composePL: Prism a b -> Lens b c -> Prism a c
composePL x y  = 
    {
        get = \structure ->  
            case x.get structure of 
                Nothing -> Nothing
                Just a -> Just (y.get a)
    ,   set = \structure value ->
            let
               struct = x.get structure  
            in
            case struct of 
                Nothing -> structure
                Just a -> x.set structure (y.set a value)             
    }



getName: Prism Model String
getName = composePL getUserData getPerUserName



{- TODO   

    - Model to email 
        Prism -> Prism -> Prism 
    - Model to Penalty
        prism -> Lens  -> Prism 
    - Model to ComprehensiveRecord
            prism -> Lens -> Prism
-}

-- Model 
type alias Model =
    {  students : List String
    ,  perUserData : Maybe PerUserData
    }

init : Model
init =  
    {   students = ["carl", "manny", "Lusi"]
    ,   perUserData = Just randomName
    }







-- Update

type Msg
    = Name
    | NoShow 

update: Msg -> Model -> Model
update msg model =
    case msg of 
        Name -> model 
        NoShow -> model






-- View 

view : Model -> Html Msg
view model =
  div []
    [
        studentList model
    ,   button [] [text "Get name"]
    ,   button [] [text "Get email"]
    ,   button [] [text "Get Penalty Amount"]
    ,   button [] [text "Get Comprehensive Record"]
    ]


createList : String -> Html msg
createList item =
    li [] [text item]

buildList : List String -> List (Html msg)
buildList lst = 
    map createList lst

studentList : Model -> Html Msg 
studentList model = 
    ol [] (buildList model.students)