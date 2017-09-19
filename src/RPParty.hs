module RPParty where

import           Control.Monad
import           Dices

data AttribTemplate = ATml {
    throw :: Throw,
    start :: Int,
    top   :: Int
  }

data MemberTemplate = MTml {
    healthTml :: AttribTemplate,
    armorTml  :: AttribTemplate,
    weaponTml :: AttribTemplate
  }

data Member = Member {
    name   :: String,
    health :: Int,
    armor  :: Int,
    weapon :: Int
  }

data Party = Party {
    title   :: String,
    members :: [Member]
  }

type PartyTemplate = [MemberTemplate]

genAttrFromTemplate :: AttribTemplate -> IO Int
genAttrFromTemplate atml = do
    dc <- cast (throw atml) (top atml)
    return (sumd dc)

genMemberFromTemplate :: String -> MemberTemplate -> IO Member
genMemberFromTemplate nm tml = do
    h <- genAttrFromTemplate (healthTml tml)
    a <- genAttrFromTemplate (armorTml tml)
    w <- genAttrFromTemplate (weaponTml tml)
    return Member { name = nm, health = h, armor = a, weapon = w }

genPartyFromTemplate :: String -> [String] -> PartyTemplate -> IO Party
genPartyFromTemplate t nms ptml = do
    ms <- zipWithM genMemberFromTemplate (nms ++ repeat "NoName") ptml
    return Party { title = t, members = ms}
