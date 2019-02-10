{-# LANGUAGE DeriveGeneric , DeriveAnyClass , DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses , OverloadedStrings  #-}

module Example.Schema
    ( gqlHandler
    )
where

import           Prelude                 hiding ( concat )
import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text
                                                , concat
                                                )
import           Data.MorpheusGraphQL           ( GQLRecord
                                                , GQLRoot
                                                , GQLArgs
                                                , (::->)(..)
                                                , GQLResponce
                                                , GQLRequest
                                                , interpreter
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Example.Files                  ( getJson )
import           Data.Aeson                     ( FromJSON )
import           Data.Either

data AddressArg = AddressArg {
    token:: Text,
    cityID:: Text
} deriving (Show,Generic,Data,GQLArgs)

data OfficeArg = OfficeArg {
    officeID:: Text
} deriving (Show,Data,Generic,GQLArgs)

data Address = Address {
        city :: Text
        ,street :: Text
        ,houseNumber :: Int
        ,owner:: Maybe User
} deriving (Generic,Show,GQLRecord,Data, FromJSON)

data User = User {
        name :: Text
        ,email :: Text
        ,address:: AddressArg ::-> Address
        ,office:: OfficeArg ::-> Address
        ,friend:: Maybe User
        ,home :: Maybe Address
} deriving (Show,Generic,Data,GQLRecord , FromJSON )

data Query = Query {
    user:: User
} deriving (Show,Generic,Data,GQLRoot, FromJSON )

fetchAddress :: Text -> Text -> IO Address
fetchAddress cityName streetName = do
    address <- getJson "address" >>= pure . fromRight (Address "" "" 0 Nothing)
    pure $ address { city   = concat [cityName, city address]
                   , street = streetName
                   }

resolveAddress :: AddressArg ::-> Address
resolveAddress = Resolver (\x -> fetchAddress (token x) (cityID x))

officeResolver :: OfficeArg -> IO Address
officeResolver args = fetchAddress (officeID args) "some bla"

userResolver :: IO User
userResolver = do
    user <- getJson "user" >>= pure . fromRight
        (User "" "" resolveAddress (Resolver officeResolver) Nothing Nothing)
    return $ user { address = resolveAddress, office = Resolver officeResolver }

rootResolver :: IO Query
rootResolver = userResolver >>= pure . Query

gqlHandler :: GQLRequest -> IO GQLResponce
gqlHandler v = do
    x <- rootResolver
    interpreter (Proxy :: Proxy Query) x v
