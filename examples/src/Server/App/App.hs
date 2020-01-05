
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.App.App where

import           Data.Text (Text, pack, unpack)
import           Data.Morpheus.Document (importGQLDocument, importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), Resolver, WithOperation)
import           Data.Morpheus.Types.Internal.AST (OperationType)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , MUTATION, QUERY, SUBSCRIPTION
                                    , Resolver(..), Undefined(..), constRes
                                    , liftEither, lift)
import Data.Morpheus (interpreter)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.IO.Class (liftIO)
import           Web.Scotty
import           Data.ByteString.Lazy (ByteString)



importGQLDocumentWithNamespace "src/Server/App/Types.gql"

type Object (o :: OperationType) e a = a(Resolver o e IO)


data Channel = USER
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)

----- API ------
lobby :: ByteString -> IO ByteString
lobby = interpreter $ lobbyGqlRoot

 
lobbyGqlRoot :: GQLRootResolver IO USEREVENT Query Mutation Subscription
lobbyGqlRoot =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  -------------------------------------------------------------
    where
      queryResolver = Query { queryName = pure Nothing }
      mutationResolver = Mutation { mutationName = pure Nothing}
      subscriptionResolver = Subscription { subscriptionName = SubResolver [] (const $ pure Nothing) }

-- resolveJoinedLobby :: SubscriptionJoinedArgs
--                    -> ResolveS USEREVENT IO UserJoined
-- resolveJoinedLobby _ =
--   SubResolver { subChannels = [USER], subResolver = subResolver }
--   where
--     subResolver (Event [USER] content) = lift (resolveJoinedLobby' content)

--     resolveJoinedLobby' :: Content -> IO (Object QUERY USEREVENT UserJoined)
--     resolveJoinedLobby' content = return
--       UserJoined { userJoinedUsername =
--                      return $ pack $ show $ contentID content
--                  }