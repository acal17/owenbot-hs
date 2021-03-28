{-# LANGUAGE OverloadedStrings #-}


module ReactSystem (messageReceivers, reactionReceivers) where

import           Discord.Types
import           Discord

import           Utils              ( newDevCommand )
import qualified Data.Text as T

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = [ reactLimit ]

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [ handleHallOfFame, handleForceOwoify ]

reactLimit :: Message -> DiscordHandler ()
reactLimit m = newDevCommand m "reactLimit *([0-9]{1,3})?" $ \captures -> do
    let parsed = readMaybe (T.unpack $ head captures)
    case parsed of
        Nothing -> do
            i <- liftIO $ readLimit
            sendMessageChan (messageChannel m)
                $ "Current limit is at " <> (T.pack $ show i)
        Just i -> do
            liftIO $ editLimit i
            sendMessageChan (messageChannel m) "New Limit Set"

editLimit :: Int -> IO ()
editLimit i = writeSingleColCSV "reactLim.csv" [T.pack $ show i]

readLimit :: IO Int
readLimit = do
    contents <- readSingleColCSV "reactLim.csv"
    if null contents
        then writeSingleColCSV "reactLim.csv" ["1"] >> pure 1
        else pure $ read $ T.unpack $ head contents

type Condition = MessageReaction -> Bool
type Handler = MessageReaction -> DiscordHandler()

-- | Handles a generic reaction event.
-- Marks it as done with a green check, and checks for its existence to prevent
-- refiring.
handleGeneric :: Condition -> Condition -> Handler -> ReactionInfo -> DiscordHandler ()
handleGeneric blockCond fufillCond handler r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> do
            let checkIfAlreadyCompleted = \x ->
                    messageReactionMeIncluded x
                    && emojiName (messageReactionEmoji x) == "✅"
            let alreadyCompleted = any checkIfCompleted reactions
            if any blockCond reactions || not (any fulfillCond reactions) || alreadyCompleted then do
                pure ()
            else do
                _ <- addReaction (reactionChannelId r) (reactionMessageId r) "✅"
                handler mess
        Left err -> liftIO (print err) >> pure ()

-- | Emote names for which to trigger force owoify on. All caps.
forceOwoifyEmotes :: [T.Text]
forceOwoifyEmotes =
    [ "BEGONEBISH" ]
        
-- | Forces the owofication of a message upon a reaction of forceOwoifyEmotes.
-- Marks it as done with a green check, and checks for its existence to prevent
-- duplicates.
handleForceOwoify :: ReactionInfo -> DiscordHandler ()
handleForceOwoify r = do
    -- Nothing to block
    let blockCond = const False
    let fulfillCond = \x ->
            T.toUpper (emojiName $ messageReactionEmoji x) `elem` forceOwoifyEmotes
            && messageReactionCount x >= limit
    let handler = \mess ->
            sendReply mess False $ owoify (messageText mess)
    handleGeneric blockCond fufillCond handler

hallOfFameEmotes :: [T.Text]
hallOfFameEmotes =
    [ "XREE"
    , "KEKW"
    , "\11088" -- star
    ] -- These are matched case-insensitively

handleHallOfFame :: ReactionInfo -> DiscordHandler ()
handleForceOwoify r = do
    let blockCond = \x ->
      reactionChannelId x `elem` hallOfFameBlockedChannels
    let fulfillCond = \x ->
            T.toUpper (emojiName $ messageReactionEmoji x) `elem` hallOfFameEmotes
            && messageReactionCount x >= limit
    let handler = \mess ->
            sendReply mess False $ owoify (messageText mess)
    handleGeneric blockCond fufillCond handler

putInHallOfFame :: Message -> DiscordHandler ()
putInHallOfFame mess = do
    embedM <- createHallOfFameEmbed mess
    case embedM of
        Right embed -> do
            sendMessageChanEmbed hallOfFameChannel "" embed
        Left err -> liftIO (putStrLn "Couldn't get link to message") >> pure ()

createDescription :: Message -> T.Text
createDescription m = messageText m <> "\n- " <> pingAuthorOf m <> " in " <> linkChannel (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
  | not . null $ messageAttachments m = attachmentUrl (head $ messageAttachments m)
  | otherwise = ""

createHallOfFameEmbed :: Message -> DiscordHandler (Either RestCallErrorCode CreateEmbed)
createHallOfFameEmbed m = do
    messLinkM <- getMessageLink m
    case messLinkM of
        Right messLink -> do
            let authorName = ""
            let authorUrl = ""
            let authorIcon = Nothing
            let embedTitle = "👑 best of ouw buwwshit"
            let embedUrl = ""
            let embedThumbnail = Nothing
            let embedDescription = (createDescription m <> "\n\n[Original Message](" <> messLink <> ")")
            let embedFields = []
            let embedImage = (Just (CreateEmbedImageUrl $ getImageFromMessage m))
            let embedFooterText = getTimestampFromMessage m
            let embedFooterIcon = Nothing
            pure $ Right (CreateEmbed authorName
                                      authorUrl
                                      authorIcon
                                      embedTitle
                                      embedUrl
                                      embedThumbnail
                                      embedDescription
                                      embedFields
                                      embedImage
                                      embedFooterText
                                      embedFooterIcon )
        Left err -> pure $ Left err
