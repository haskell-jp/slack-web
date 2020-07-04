{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Channel
-- Description: Types and functions related to <https://api.slack.com/docs/conversations-api Conversation API>
--
--
--
----------------------------------------------------------------------

module Web.Slack.Conversation
  ( Conversation(..)
  , ConversationId(..)
  , ChannelConversation(..)
  , GroupConversation(..)
  , ImConversation(..)
  , TeamId(..)
  , Purpose(..)
  , Topic(..)
  ) where

-- aeson
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- unordered-containers
import qualified Data.HashMap.Strict as HM

-- base
import GHC.Generics (Generic)

-- http-api-data
-- import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Common
import Web.Slack.Types
import Web.Slack.Util

-- scientific
import Data.Scientific

-- text
import Data.Text (Text)


-- |
--
--
data Topic =
  Topic
    { topicValue :: Text
    , topicCreator :: Text
    , topicLastSet :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "topic") ''Topic)


-- |
--
--
data Purpose =
  Purpose
    { purposeValue :: Text
    , purposeCreator :: Text
    , purposeLastSet :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "purpose") ''Purpose)


-- | Conversation object representing a public channel,
--   which any people in the team can join in and see.
data ChannelConversation =
  ChannelConversation
    { channelId :: ConversationId
    , channelName :: Text
    , channelCreated :: Integer
    , channelIsArchived :: Bool
    , channelIsGeneral :: Bool
    , channelUnlinked :: Integer
    , channelNameNormalized :: Text
    , channelIsShared :: Bool

    -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , channelParentConversation: null
    , channelCreator :: UserId
    , channelIsExtShared :: Bool
    , channelIsOrgShared :: Bool
    , channelSharedTeamIds :: [TeamId]

    -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , channelPendingShared :: [TeamId]
    -- , channelPendingConnectedTeamIds :: [TeamId]

    , channelIsPendingExtShared :: Bool
    , channelIsMember :: Bool
    , channelTopic :: Topic
    , channelPurpose :: Purpose
    , channelPreviousNames :: [Text]
    , channelNumMembers :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "channel") ''ChannelConversation)


-- | Conversation object representing a private channel or
--   _a multi-party instant message (mpim)*, which only invited people in the
--  team can join in and see.
data GroupConversation =
  GroupConversation
    { groupId :: ConversationId
    , groupName :: Text
    , groupCreated :: Integer
    , groupIsArchived :: Bool
    , groupIsGeneral :: Bool
    , groupUnlinked :: Integer
    , groupNameNormalized :: Text
    , groupIsShared :: Bool

    -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , groupParentConversation :: null

    , groupCreator :: UserId
    , groupIsExtShared :: Bool
    , groupIsOrgShared :: Bool
    , groupSharedTeamIds :: [TeamId]

    -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , group_pending_shared :: []
    -- , group_pending_connected_team_ids :: []

    , groupIsPendingExtShared :: Bool
    , groupIsMember :: Bool
    , groupIsPrivate :: Bool
    , groupIsMpim :: Bool
    , groupLastRead :: SlackTimestamp
    , groupIsOpen :: Bool
    , groupTopic :: Topic
    , groupPurpose :: Purpose
    , groupPriority :: Scientific
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "group") ''GroupConversation)


-- | Conversation object representing a (single-party) instance message,
--   where only two people talk.
data ImConversation =
  ImConversation
    { imId :: ConversationId
    , imName :: Text
    , imCreated :: Integer
    , imIsArchived :: Bool
    , imIsGeneral :: Bool
    , imUnlinked :: Integer
    , imNameNormalized :: Text
    , imIsShared :: Bool

    -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , im_parent_conversation :: null

    , imCreator :: UserId
    , imIsExtShared :: Bool
    , imIsOrgShared :: Bool
    , imSharedTeamIds :: [TeamId]

    -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- im_pending_shared :: []
    -- im_pending_connected_team_ids :: []

    , imIsPendingExtShared :: Bool
    , imIsMember :: Bool
    , imIsPrivate :: Bool
    , imLastRead :: SlackTimestamp
    , imIsOpen :: Bool
    , imTopic :: Topic
    , imPurpose :: Purpose
    , imPriority :: Scientific
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "im") ''ImConversation)


-- | Ref. https://api.slack.com/types/conversation
--
--
data Conversation =
      Channel ChannelConversation
    | Group GroupConversation
    | Im ImConversation
  deriving (Eq, Generic, Show)


instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o -> do
    isChannel <- o .: "is_channel"
    if isChannel
      then Channel <$> parseJSON (Object o)
      else do
        isGroup <- o .: "is_group"
        if isGroup
          then Group <$> parseJSON (Object o)
          else do
            isIm <- o .: "is_im"
            if isIm
              then Im <$> parseJSON (Object o)
              else
                prependFailure
                  "parsing a Conversation failed: neither channel, group, nor im, "
                  (typeMismatch "Conversation" (Object o))


instance ToJSON Conversation where
  toJSON (Channel channel) =
    let (Object obj) = toJSON channel
     in Object
          . HM.insert "is_channel" (Bool True)
          . HM.insert "is_group" (Bool False)
          $ HM.insert "is_im" (Bool False) obj
  toJSON (Group group) =
    let (Object obj) = toJSON group
     in Object
          . HM.insert "is_channel" (Bool False)
          . HM.insert "is_group" (Bool True)
          $ HM.insert "is_im" (Bool False) obj
  toJSON (Im im) =
    let (Object obj) = toJSON im
     in Object
          . HM.insert "is_channel" (Bool False)
          . HM.insert "is_group" (Bool False)
          $ HM.insert "is_im" (Bool True) obj
