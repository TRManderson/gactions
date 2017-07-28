{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module API where
import Data.String (IsString, fromString)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.HashMap.Lazy as M hiding (filter)
import Data.Text hiding (filter, head)
import Data.Foldable (asum)
import Data.Bool (bool)
import Control.Applicative ((<|>))
-- import Control.Monad.Fail as M

replaceStr :: String -> String -> (String -> String)
replaceStr s1 s2 = \s -> bool s s2 (s == s1)

data Permissions = UnspecifiedPermission | NamePermission | PreciseLocationPermission | CoarseLocationPermission deriving (Eq)
permissionsPair = [ (UnspecifiedPermission, "UNSPECIFIED_PERMISSION")
                  , (NamePermission, "NAME")
                  , (PreciseLocationPermission, "DEVICE_PRECISE_LOCATION")
                  , (CoarseLocationPermission, "DEVICE_COARSE_LOCATION")
                  ]
instance ToJSON Permissions where
  toJSON = sumTypeToJSON permissionsPair

instance FromJSON Permissions where
  parseJSON = sumTypeParseJSON "Permissions" permissionsPair


data UserProfile = UserProfile { displayName :: Text
                               , givenName :: Text
                               , familyName :: Text
                               } deriving (Eq, Generic)
instance ToJSON UserProfile
instance FromJSON UserProfile

data User = User { userId :: Text
                 , profile :: UserProfile
                 , accessToken :: Text
                 , permissions :: [Permissions]
                 , locale :: Text
                 } deriving (Eq, Generic)
instance ToJSON User
instance FromJSON User

data Device = Device { location :: Location } deriving (Eq, Generic)
instance ToJSON Device
instance FromJSON Device

data Surface = Surface { capabilities :: [Capability]}  deriving (Eq, Generic)
instance ToJSON Surface
instance FromJSON Surface

data Capability = Capability { name :: Text }  deriving (Eq, Generic)
instance ToJSON Capability
instance FromJSON Capability

data ConversationType = UnspecifiedConversation | NewConversation | ActiveConversation deriving (Eq)
conversationTypePairs = [ (UnspecifiedConversation, "TYPE_UNSPECIFIED")
                        , (NewConversation, "NEW")
                        , (ActiveConversation, "ACTIVE")
                        ]
instance ToJSON ConversationType where
    toJSON = sumTypeToJSON conversationTypePairs
instance FromJSON ConversationType where
    parseJSON = sumTypeParseJSON "ConversationType" conversationTypePairs

data Conversation = Conversation { conversationId :: Text
                                 , conversationType :: ConversationType
                                 , conversationToken :: Text
                                 } deriving (Eq, Generic)
instance ToJSON Conversation where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = replaceStr "conversationType" "type"})
instance FromJSON Conversation where
    parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = replaceStr "type" "conversationType"})

data Input = Input { rawInputs :: [RawInput]
                   , intent :: Text
                   , arguments :: [Argument]
                   } deriving (Eq, Generic)
instance ToJSON Input
instance FromJSON Input

data TimeOfDay = Time { hours :: Int, minutes :: Int, seconds :: Int, nanos :: Int} deriving (Eq, Generic)
data Date = Date {year :: Int, month :: Int, day :: Int} deriving (Eq, Generic)
data DateTime = DateTime { date :: Date, time :: TimeOfDay } deriving (Eq, Generic)

instance ToJSON TimeOfDay
instance ToJSON Date
instance ToJSON DateTime
instance FromJSON TimeOfDay
instance FromJSON Date
instance FromJSON DateTime

data Argument = ArgumentBool BoolArgument | ArgumentText TextArgument | ArgumentDateTime DateTimeArgument deriving (Eq)
instance ToJSON Argument where
  toJSON (ArgumentBool a) = toJSON a
  toJSON (ArgumentText a) = toJSON a
  toJSON (ArgumentDateTime a) = toJSON a
instance FromJSON Argument where
  parseJSON = withObject "Argument" $ \obj ->
    asum (fmap (appl obj) prs) <|> fail "Invalid argument"
      where
        appl obj (k, mkP) = bool (fail "Nope") (mkP $ Object obj) (k `elem` keys obj)
        prs = [ ("boolValue", (fmap.fmap) ArgumentBool parseJSON)
              , ("textValue", (fmap.fmap) ArgumentText parseJSON)
              , ("datetimeValue", (fmap.fmap) ArgumentDateTime parseJSON)
              ]



data BoolArgument = BoolArgument { name :: Text
                                 , rawText :: Text
                                 , boolValue :: Bool
                                 , extension :: Object
                                 } deriving (Eq, Generic)

data TextArgument = TextArgument { name :: Text
                                 , rawText :: Text
                                 , textValue :: Text
                                 , extension :: Object
                                 } deriving (Eq, Generic)

data DateTimeArgument = DateTimeArgument { name :: Text
                                         , rawText :: Text
                                         , datetimeValue :: DateTime
                                         , extension :: Object
                                         } deriving (Eq, Generic)

instance ToJSON BoolArgument
instance ToJSON TextArgument
instance ToJSON DateTimeArgument
instance FromJSON BoolArgument
instance FromJSON TextArgument
instance FromJSON DateTimeArgument

data RawInput = RawInput { createTime :: Text
                         , inputType :: InputType
                         , query :: String
                         } deriving (Eq, Generic)
instance ToJSON RawInput
instance FromJSON RawInput

data InputType = UnspecifiedInput | KeyboardInput | VoiceInput | TouchInput deriving (Eq)
instance ToJSON InputType where
  toJSON UnspecifiedInput = String "UNSPECIFIED_INPUT_TYPE"
  toJSON KeyboardInput = String "KEYBOARD"
  toJSON VoiceInput = String "VOICE"
  toJSON TouchInput = String "TOUCH"
instance FromJSON InputType where
  parseJSON = withText "InputType" $ \txt ->
    case txt of
      "UNSPECIFIED_INPUT_TYPE" -> pure UnspecifiedInput
      "KEYBOARD" -> pure KeyboardInput
      "VOICE" -> pure VoiceInput
      "TOUCH" -> pure TouchInput
      _ -> fail "Invalid string"


data AppRequest = AppRequest { user :: User
                             , device :: Device
                             , surface :: Surface
                             , conversation :: Conversation
                             , inputs :: [Input]
                             , isInSandbox :: Bool
                             } deriving (Eq, Generic)
instance ToJSON AppRequest
instance FromJSON AppRequest

type InputPrompt = Object
type ExpectedIntent = Object
type RichResponse = Object
type ExpectedInput = Object
type FinalResponse = Object
type CustomPushMessage = Object
type ResponseMetadata = Object

data AppResponse = AppResponse { conversationToken :: String
                               , expectUserResponse :: Bool
                               , expectedInputs :: [ExpectedInput]
                               , finalResponse :: FinalResponse
                               , customPushMessage :: CustomPushMessage
                               , responseMetadata :: ResponseMetadata
                               , isInSandbox :: Bool
                               } deriving (Eq, Generic)
instance ToJSON AppResponse
instance FromJSON AppResponse


data SurfaceRequirements = SurfaceRequirements { minimumCapabilities :: [Capability] } deriving (Eq, Generic)
instance ToJSON SurfaceRequirements
instance FromJSON SurfaceRequirements


type Location = Object



sumTypeToJSON :: Eq a => [(a, Text)] -> a -> Value
sumTypeToJSON prs a = String . snd . head . filter ((== a) . fst) $ prs

sumTypeParseJSON :: String -> [(a, Text)] -> Value -> Parser a
sumTypeParseJSON name prs = withText name $ \txt ->
  case filter ((==) txt . snd) prs of
    x:_ -> pure $ fst x
    _ -> fail "Invalid"

data Voice = MaleVoice1 | MaleVoice2 | FemaleVoice1 | FemaleVoice2 deriving (Eq)
voicePairs = [ (MaleVoice1, "male_1")
             , (MaleVoice2, "male_2")
             , (FemaleVoice1, "female_1")
             , (FemaleVoice2, "female_2")
             ]

instance ToJSON Voice where
  toJSON = sumTypeToJSON voicePairs
instance FromJSON Voice where
  parseJSON = sumTypeParseJSON "Voice" voicePairs

data Manifest = Manifest { displayName :: Text
                         , invocationName :: Text
                         , shortDescription :: Text
                         , longDescription :: Text
                         , category :: Text
                         , smallSquareLogoUrl :: Text
                         , largeLandscapeLogoUrl :: Text
                         , companyName :: Text
                         , contactEmail :: Text
                         , termsOfServiceUrl :: Text
                         , privacyUrl :: Text
                         , sampleInvocation :: [Text]
                         , introduction :: Text
                         , testingInstructions :: Text
                         , voice :: Voice
                         , surfaceRequirements :: SurfaceRequirements
                         } deriving (Eq, Generic)

instance ToJSON Manifest where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = replaceStr "voice" "voiceName"})
instance FromJSON Manifest where
    parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = replaceStr "voiceName" "voice"})


data AssertionType = UnknownAssertion | IDTokenAssertion | AccountCreationAssertion deriving Eq
assertionTypePairs = [ (UnknownAssertion, "UNKNOWN_ASSERTION_TYPE")
                     , (IDTokenAssertion, "ID_TOKEN")
                     , (AccountCreationAssertion, "ACCOUNT_CREATION")
                     ]
instance ToJSON AssertionType where toJSON = sumTypeToJSON assertionTypePairs
instance FromJSON AssertionType where parseJSON = sumTypeParseJSON "AssertionType" assertionTypePairs

data AuthGrantType = UnspecifiedGrantType | AuthCodeGrantType | ImplicitGrantType deriving Eq
authGrantTypePairs = [ (UnspecifiedGrantType, "AUTH_GRANT_TYPE_UNSPECIFIED")
                     , (AuthCodeGrantType, "AUTH_CODE")
                     , (ImplicitGrantType, "IMPLICIT")
                     ]
instance ToJSON AuthGrantType where toJSON = sumTypeToJSON authGrantTypePairs
instance FromJSON AuthGrantType where parseJSON = sumTypeParseJSON "AuthGrantType" authGrantTypePairs


data AccountLinking = AccountLinking { clientId :: Text
                                     , clientSecret :: Text
                                     , grantType :: AuthGrantType
                                     , authenticationUrl :: Text
                                     , accessTokenUrl :: Text
                                     , scopes :: [Text]
                                     , scopeExplanationUrl :: Text
                                     , googleSignInClientId :: Text
                                     , assertionTypes :: [AssertionType]
                                     } deriving (Eq, Generic)
instance ToJSON AccountLinking
instance FromJSON AccountLinking

data Fulfillment = Fulfillment { conversationName :: Text } deriving (Eq, Generic)
instance ToJSON Fulfillment
instance FromJSON Fulfillment

data Parameter = Parameter { name :: Text, paramType :: Text} deriving (Eq, Generic)
instance ToJSON Parameter where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = replaceStr "paramType" "type"})
instance FromJSON Parameter where
    parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = replaceStr "type" "paramType"})

data Trigger = Trigger { queryPatterns :: [Text] } deriving (Eq, Generic)
instance ToJSON Trigger
instance FromJSON Trigger

data Intent = Intent { name :: Text
                     , parameters :: [Parameter]
                     , trigger :: Trigger
                     } deriving (Eq, Generic)
instance ToJSON Intent
instance FromJSON Intent

data Action = Action { name :: Text
                     , fulfillment :: Fulfillment
                     , intent :: Intent
                     , description :: Text
                     , signInRequired :: Bool
                     } deriving (Eq, Generic)
instance ToJSON Action
instance FromJSON Action



data ActionsType = ActionsType { name :: Text
                             , entities :: [Entity]
                             , isUserDefined :: Bool
                             } deriving (Eq, Generic)
instance ToJSON ActionsType
instance FromJSON ActionsType

data Entity = Entity { key :: Text, syonyms :: [Text]} deriving (Eq, Generic)
instance ToJSON Entity
instance FromJSON Entity

data ConversationFulfillment = ConversationFulfillment { name :: Text
                                                       , url :: Text
                                                       , httpHeaders :: HashMap Text Textl
                                                       , fulfillmentApiVersion :: Int
                                                       } deriving (Eq, Generic)
instance ToJSON ConversationFulfillment
instance FromJSON ConversationFulfillment

data ActionPackage = ActionPackage { manifest :: Manifest
                                   , accountLinking :: AccountLinking
                                   , actions :: [Action]
                                   , types :: [ActionsType]
                                   , conversations :: HashMap Text ConversationFulfillment
                                   }  deriving (Eq, Generic)
instance ToJSON ActionPackage
instance FromJSON ActionPackage


data ActionType = ActionType