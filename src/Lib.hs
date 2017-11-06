{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson hiding ((.:))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Digestive
import Text.Digestive.Aeson

-- DOMAIN TYPES
-- the field names here must match the field names in our form
data CreateUserReq = CreateUserReq
    { name :: Text
    , email :: Text
    } deriving (Show, Generic)

instance ToJSON CreateUserReq

data User = User
    { userName :: Text
    , userEmail :: Text
    }

--
-- FORM
-- the field names here must match the field names of whatever we are
-- running our form against.
userForm :: Monad m => Form Text m User
userForm =
    pure User <*>
    "name" .: check "Name cannot be empty" checkName (text Nothing) <*>
    "email" .: check "Not a valid email address" checkEmail (text Nothing)

checkName :: Text -> Bool
checkName name
    | name == "" = False
    | otherwise = True

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

validCreateUserReq :: CreateUserReq
validCreateUserReq = CreateUserReq "Steven" "email@example.com"

invalidCreateUserReq :: CreateUserReq
invalidCreateUserReq = CreateUserReq "" "email"

main :: IO ()
main = do
    (validView, _) <- digestJSON userForm (toJSON validCreateUserReq)
    let withoutErrors = jsonErrors validView
    -- putStrLn (show $ toEncoding withoutErrors); no errors will break
    (invalidView, _) <- digestJSON userForm (toJSON invalidCreateUserReq)
    let withErrors = jsonErrors invalidView
    putStrLn (show $ toEncoding withErrors)
    return ()
