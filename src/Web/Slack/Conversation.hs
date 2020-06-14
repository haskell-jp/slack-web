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
  , TeamId(..)
  , Purpose(..)
  , Topic(..)
  ) where

-- aeson
import Data.Aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- http-api-data
import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Common
import Web.Slack.Util

-- scientific
import Data.Scientific

-- text
import Data.Text (Text)

-- | Ref. https://api.slack.com/types/conversation
--
--
data Conversation =
      Channel ChannelConversation
    | Group GroupConversation
    | Im ImConversation
  deriving (Eq, Generic, Show)


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


-- | Conversation object representing a (single-party) instance message,
--   where only two people talk.
data ImConversation =
  ImConversation
    { imId :: ConversationId
    , imName :: Text
    , imIsChannel :: Bool
    , imIsGroup :: Bool
    , imIsIm :: Bool
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
