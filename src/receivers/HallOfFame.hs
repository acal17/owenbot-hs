{-# LANGUAGE OverloadedStrings #-}

module HallOfFame ( reactionReceivers, messageReceivers ) where

import           Control.Monad      ( guard
                                    , when
                                    )
import           Data.Functor       ( (<&>) )
import qualified Data.Text as T
import           Discord            ( restCall
                                    , DiscordHandler
                                    , RestCallErrorCode
                                    )
import qualified Discord.Requests as R
import           Discord.Types      ( ChannelId
                                    , Snowflake
                                    , Attachment ( attachmentUrl )
                                    , Emoji ( emojiName )
                                    , Message ( messageReactions
                                              , messageId
                                              , messageText
                                              , messageChannel
                                              , messageAttachments
                                              , messageChannel
                                              )
                                    , MessageReaction ( messageReactionCount
                                                      , messageReactionEmoji
                                                      )
                                    , CreateEmbed ( CreateEmbed )
                                    , CreateEmbedImage ( CreateEmbedImageUrl )
                                    , ReactionInfo ( reactionEmoji
                                                   , reactionChannelId
                                                   , reactionMessageId
                                                   )
                                    )
import           Text.Read          ( readMaybe )
import           UnliftIO           ( liftIO )

import           Utils              ( sendMessageChan
                                    , pingAuthorOf
                                    , messageFromReaction
                                    , linkChannel
                                    , getMessageLink
                                    , sendMessageChanEmbed
                                    , getTimestampFromMessage
                                    , newDevCommand
                                    )
import           CSV                ( readSingleColCSV
                                    , writeSingleColCSV
                                    )

attemptHallOfFame :: ReactionInfo -> DiscordHandler ()
attemptHallOfFame r = do
    when (isHallOfFameEmote r && notInHallOfFameChannel r) $ do
        eligible <- isEligibleForHallOfFame r
        when eligible $ putInHallOfFame r

hallOfFameEmotes :: [T.Text]
hallOfFameEmotes =
    [ "XREE"
    , "KEKW"
    , "\11088" -- star
    ] -- These are matched case-insensitively

hallOfFameChannel :: ChannelId
hallOfFameChannel = 790936269382615082 --the channel id for the hall of fame

notInHallOfFameChannel :: ReactionInfo -> Bool
notInHallOfFameChannel r = reactionChannelId r /= hallOfFameChannel

isHallOfFameEmote :: ReactionInfo -> Bool
isHallOfFameEmote r = T.toUpper (emojiName (reactionEmoji r)) `elem` hallOfFameEmotes

isEligibleForHallOfFame :: ReactionInfo -> DiscordHandler Bool
isEligibleForHallOfFame r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> do
            msgIdList <- liftIO $ readSingleColCSV "fame.csv"
            let msgIdListStr = T.unpack <$> msgIdList
            limit <- liftIO readLimit
            let existsInHOF = show (messageId mess) `elem` msgIdListStr
            let reactions = messageReactions mess
            let capsEmotes = map T.toUpper hallOfFameEmotes
            let fulfillCond = \x ->
                    (messageReactionCount x) >= limit
                    && T.toUpper (emojiName $ messageReactionEmoji x) `elem` capsEmotes
                    && not existsInHOF
            pure $ any fulfillCond reactions
        Left err -> liftIO (putStrLn (show err)) >> pure False

putInHallOfFame :: ReactionInfo -> DiscordHandler ()
putInHallOfFame r = do
    messM <- messageFromReaction r --gets contents of message that was reacted to.
    case messM of
        Right mess -> do
            embedM <- createHallOfFameEmbed mess
            case embedM of
                Right embed -> do
                    msgIdList <- liftIO $ readSingleColCSV "fame.csv"
                    liftIO $ writeSingleColCSV "fame.csv" ((T.pack $ show $ messageId mess):msgIdList)
                    --adds the message id to the csv to make sure we dont add it multiple times.
                    sendMessageChanEmbed hallOfFameChannel "" embed
                Left err -> liftIO (putStrLn "Couldn't get link to message") >> pure ()
        Left err -> liftIO (putStrLn "Couldn't find associated message") >> pure ()