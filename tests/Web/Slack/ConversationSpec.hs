{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Slack.ConversationSpec
  ( spec
  ) where

import Data.Maybe

-- hspec
import Test.Hspec

-- QuickCheck
import Test.QuickCheck

-- quickcheck-instances
import Test.QuickCheck.Instances ()

import Web.Slack.Common
import Web.Slack.Conversation


instance Arbitrary Slack.MessageType where
  arbitrary = return Slack.MessageTypeMessage

instance Arbitrary Slack.UserId where
  arbitrary = fromJust . decode . encode <$> (arbitrary :: Gen T.Text)

deriving instance Arbitrary SlackMessageText

instance Arbitrary Slack.SlackTimestamp where
  -- NOTE: `arbitrary :: Gen UTCTime` only for positive POSIX Second is really slow!
  arbitrary = Slack.mkSlackTimestamp . posixSecondsToUTCTime . fromIntegral <$> (arbitrary :: Gen Word)

instance Arbitrary Message where
  arbitrary =
    Slack.Message <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


spec :: Spec
spec = describe "ToJSON and FromJSON for Conversation" $ do
    prop "the encoded json is decoded as " $ \conversation -> do
      actual <- either fail return . eitherDecode $ encode msg
      actual `shouldBe` (conversation :: Conversation)
