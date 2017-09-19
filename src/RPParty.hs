module RPParty where

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
    health :: Int,
    armor  :: Int,
    weapon :: Int
  }

type Party = [Member]

genAttrFromTemplate :: AttribTemplate -> IO Int
genAttrFromTemplate atml = do
    dc <- cast (throw atml) (top atml)
    return (sumd dc)

genMemberFromTemplate :: MemberTemplate -> IO Member
genMemberFromTemplate tml = do
    h <- genAttrFromTemplate (healthTml tml)
    a <- genAttrFromTemplate (armorTml tml)
    w <- genAttrFromTemplate (weaponTml tml)
    return Member { health = h, armor = a, weapon = w }
