import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Either exposing (..)
import Char exposing (..)
import String exposing (..)

main =
    Html.beginnerProgram { model = model, view = view, update = update}

-- MODEL

type alias Model =
    {   name                : String
    ,   password            : String
    ,   passwordAgain       : String
    ,   age                 : String
    ,   validationStatus    : (String, String)
    }

model : Model
model = Model "" "" "" "" ("", "")

-- UPDATE

type Msg
    =   Name String
    |   Password String
    |   PasswordAgain String
    |   Age String
    |   Submit

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name newName ->
            { model | name = newName }
        Password newPassword ->
            { model | password = newPassword }
        PasswordAgain newPasswordAgain ->
            { model | passwordAgain = newPasswordAgain }
        Age newAge ->
            { model | age = newAge }
        Submit ->
            { model | validationStatus = calcValidationStatus model }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [   input   [ type_ "text", placeholder "Name", onInput Name ] []
        ,   input   [ type_ "password", placeholder "Password", onInput Password ] []
        ,   input   [ type_ "password", placeholder "Re-enter password", onInput PasswordAgain ] []
        ,   input   [ type_ "text", placeholder "Age", onInput Age ] []
        ,   button  [ onClick Submit ] [ text "Submit" ]
        ,   viewValidation model
        ]

-- VALIDATION

type alias ValidationResult = Either (String, String) (String, String)

calcValidationStatus : Model -> (String, String)
calcValidationStatus model =
    unpack identity identity
    ((identityValidation
        |> lengthValidation
        |> allowedSymbolsValidation
        |> ageValidation) model)

viewValidation : Model -> Html Msg
viewValidation model =
    case model.validationStatus of
        ("", "") ->
            div [] []
        (color, message) ->
            div [ style [("color", color)] ] [ text message ]

identityValidation : Model -> ValidationResult
identityValidation model =
    if model.password == model.passwordAgain then
        Right ("green", "OK")
    else
        Left ("red", "Passwords do not match!")

lengthValidation : (Model -> ValidationResult) -> Model -> ValidationResult
lengthValidation prev model =
    case (prev model) of
        Right tuple ->
            if (String.length model.password) > 8 then
                Right tuple
            else
                Left ("red", "Password too short!")
        Left tuple ->
            Left tuple

allowedSymbolsValidation : (Model -> ValidationResult) -> Model -> ValidationResult
allowedSymbolsValidation prev model =
    let
        allowed : Char -> Bool
        allowed c =
            (isDigit c) || (isUpper c) || (isLower c)
    in
    case (prev model) of
            Right tuple ->
                if (all allowed model.password) then
                    Right tuple
                else
                    Left ("red", "Password must be alphanumeric!")
            Left tuple ->
                Left tuple

ageValidation : (Model -> ValidationResult) -> Model -> ValidationResult
ageValidation prev model =
    case (prev model) of
        Right tuple ->
            if (all isDigit model.age) then
                Right tuple
            else
                Left ("red", "Age must be numer!")
        Left tuple ->
            Left tuple