{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}

module API where


import Data.Aeson

{- JSON API
{
  "user": {
    object(User)
  },
  "device": {
    object(Device)
  },
  "surface": {
    object(Surface)
  },
  "conversation": {
    object(Conversation)
  },
  "inputs": [
    {
      object(Input)
    }
  ],
  "isInSandbox": boolean,
}
-}

type Permissions = Object
data UserProfile = UserProfile { displayName :: String
                               , givenName :: String
                               , familyName :: String
                               }

data User = User { userId :: String
                 , profile :: UserProfile
                 , accessToken :: String
                 , permissions :: [Permissions]
                 , locale :: String
                 }
type Device = Object
type Surface = Object
type Conversation = Object
type Input = Object

data AppRequest = AppRequest { user :: User
                             , device :: Device
                             , surface :: Surface
                             , conversation :: Conversation
                             , inputs :: [Input]
                             , isInSandbox :: Bool
                             }

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
                               }

-- type AppEndpoint = ReqBody '[JSON] AppRequest :> Post '[JSON] AppResponse
