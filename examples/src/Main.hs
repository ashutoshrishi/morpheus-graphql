{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Main
  ( main
  )
where


import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor.Identity          ( Identity(..) )

import           Data.Morpheus.Types            ( GQLRootResolver )
import           Data.Morpheus                  ( Interpreter(..) )
import           Data.Morpheus.Document         ( toGraphQLDocument )
import           Data.Morpheus.Server           ( GQLState
                                                , gqlSocketApp
                                                , initGQLState
                                                , RootResCon
                                                )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWs
import           Network.WebSockets             ( defaultConnectionOptions )
import           Web.Scotty                     ( body
                                                , file
                                                , get
                                                , post
                                                , raw
                                                , scottyApp
                                                , ScottyM
                                                , RoutePattern
                                                )

-- examples
import           Client.Client                  ( fetUser
                                                , fetchHero
                                                )
import           Server.Mythology.API           ( mythologyApi )
import           Server.TH.Simple               ( thSimpleApi )
import           Server.Sophisticated.API       ( EVENT
                                                , gqlRoot
                                                )
import           Server.App.App                 (lobby)
import           Data.ByteString.Lazy           (ByteString)



endpoint :: RoutePattern -> (ByteString -> IO ByteString)-> ScottyM ()
endpoint path app = do
  get  path $ file "./examples/index.html"
  post path $ raw =<< (liftIO . app =<< body)

endpointPubSub :: RootResCon IO e q m s => RoutePattern -> GQLState IO e -> GQLRootResolver IO e q m s -> ScottyM ()
endpointPubSub path state rootRes = do 
    get path $ file "./examples/index.html"
    post path $ raw =<< (liftIO . interpreter rootRes state =<< body)

main :: IO ()
main = do
  state   <- initGQLState
  httpApp <- httpServer state
  fetchHero >>= print
  fetUser (interpreter gqlRoot state) >>= print
  Warp.runSettings settings
    $ WaiWs.websocketsOr defaultConnectionOptions (wsApp state) httpApp
 where
  settings = Warp.setPort 3000 Warp.defaultSettings
  wsApp    = gqlSocketApp gqlRoot
  httpServer :: GQLState IO EVENT -> IO Wai.Application
  httpServer state = scottyApp $ do
    get "/schema.gql" $ raw $ toGraphQLDocument $ Identity gqlRoot
    endpointPubSub "/" state gqlRoot
    endpoint "/mythology" mythologyApi
    endpoint "/th" thSimpleApi
    endpoint "lobby" lobby