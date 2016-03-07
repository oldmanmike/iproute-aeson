{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import            Control.Monad
import            Data.IP
import            Data.Aeson
import            Data.IP.Aeson
import            Data.Word
import            GHC.Generics
import            Test.QuickCheck


data ExtractedIPv4 = ExtractedIPv4
  { ipv4:: IPv4
  } deriving (Show,Eq,Generic)


data ExtractedIPv6 = ExtractedIPv6
  { ipv6 :: IPv6
  } deriving (Show,Eq,Generic)


instance ToJSON ExtractedIPv4
instance FromJSON ExtractedIPv4
instance ToJSON ExtractedIPv6
instance FromJSON ExtractedIPv6


instance Arbitrary IPv4 where
  arbitrary = do
    let a = fmap fromEnum (arbitrary :: Gen Word8)
    let b = fmap fromEnum (arbitrary :: Gen Word8)
    let c = fmap fromEnum (arbitrary :: Gen Word8)
    let d = fmap fromEnum (arbitrary :: Gen Word8)
    liftM toIPv4 (sequence [a,b,c,d])


instance Arbitrary IPv6 where
  arbitrary = do
    let a = fmap fromEnum (arbitrary :: Gen Word16)
    let b = fmap fromEnum (arbitrary :: Gen Word16)
    let c = fmap fromEnum (arbitrary :: Gen Word16)
    let d = fmap fromEnum (arbitrary :: Gen Word16)
    let e = fmap fromEnum (arbitrary :: Gen Word16)
    let f = fmap fromEnum (arbitrary :: Gen Word16)
    let g = fmap fromEnum (arbitrary :: Gen Word16)
    let h = fmap fromEnum (arbitrary :: Gen Word16)
    liftM toIPv6 (sequence [a,b,c,d,e,f,g,h])


prop_IdentityJSONIPv4 :: IPv4 -> Bool
prop_IdentityJSONIPv4 ip = (Just testData) == decode (encode testData)
  where testData = ExtractedIPv4 ip


prop_IdentityJSONIPv6 :: IPv6 -> Bool
prop_IdentityJSONIPv6 ip = (Just testData) == decode (encode testData)
  where testData = ExtractedIPv6 ip


main :: IO ()
main = do
  quickCheck prop_IdentityJSONIPv4
  quickCheck prop_IdentityJSONIPv6
