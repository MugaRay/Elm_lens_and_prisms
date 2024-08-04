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



-- Model 
type alias Model =
    {  students : List String
    ,  perUserData : Maybe PerUserData
    }



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




-- Compose functons 
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

composePP: Prism a b -> Prism b c -> Prism a c 
composePP x y = 
    {
        get = \structure ->
            case x.get structure of
                Nothing -> Nothing
                Just a -> 
                    case y.get a of
                        Nothing -> Nothing
                        Just b -> Just b
    ,   set = \structure value ->
                let
                    struct = x.get structure
                in
                    case struct of 
                        Nothing -> structure 
                        Just a -> x.set structure (y.set a value)
    }



-- Lens and prism implementations
userName: Lens PerUserData String
userName = 
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


comprehensivePenalty: Lens ComprehensiveRecord Float 
comprehensivePenalty =
    {
        get = \structure -> structure.penaltyAmount
    ,   set = \structure value -> {structure | penaltyAmount = value}
    }


getComprehensiveRecord : Prism PerUserData ComprehensiveRecord
getComprehensiveRecord = 
    {
        get = \structure ->
            case structure of 
                BasicData _ -> Nothing 
                ComprehensiveRecordData v -> Just v
    ,   set = \structure value ->
            case structure of
                BasicData _ -> structure 
                ComprehensiveRecordData _ -> ComprehensiveRecordData value
    }



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




-- compositions 

perUserName: Prism Model String
perUserName = composePL getUserData userName

perUserComprensiveRecord: Prism Model ComprehensiveRecord
perUserComprensiveRecord = composePP getUserData getComprehensiveRecord

perUserPenality: Prism Model Float 
perUserPenality = composePL perUserComprensiveRecord comprehensivePenalty



-- Get User Index
getStudentAt: List String -> Int -> Maybe String 
getStudentAt lst index =
    let innerRec : Int -> List String -> Maybe String
        innerRec curr l =
            case l of 
                [] -> Nothing
                x::xs -> 
                    if curr == index then 
                        Just x
                    else
                        innerRec (curr + 1) xs 
    in 
        innerRec 0 lst


-- init 

randomName : PerUserData
randomName = BasicData {
        name = "carl"
    ,   email = Just "Hello"
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