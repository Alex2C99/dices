module RPParty where

import           Control.Monad
import           Data.List
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
  } deriving (Eq)

data Party = Party {
    title   :: String,
    members :: [Member]
  }

type PartyTemplate = [MemberTemplate]

noname :: String
noname = "NoName"

isAlive :: Member -> Bool
isAlive mb = health mb > 0

isHit :: Member -> IO Bool
isHit mb = do
    (Single d) <- cast One 20
    return (d > armor mb)

getDamage :: Member -> Int -> Member
getDamage mb dmg = if isAlive mb
    then mb { health = if health mb - dmg > 0 then health mb - dmg else 0 }
    else mb

doHit :: Member -> Member -> IO Member
doHit attaker target = do
    hit <- isHit target
    if hit
        then do
            (Single dmg) <- cast One (weapon attaker)
            return (getDamage target dmg)
        else return target

getMaxHealth :: [Member] -> Member
getMaxHealth = maximumBy (\a b -> compare (health a) (health b))

genAttrFromTemplate :: AttribTemplate -> IO Int
genAttrFromTemplate atml = do
    dc <- cast (throw atml) (top atml)
    return (start atml + sumd dc)

genMemberFromTemplate :: String -> MemberTemplate -> IO Member
genMemberFromTemplate nm tml = do
    h <- genAttrFromTemplate (healthTml tml)
    a <- genAttrFromTemplate (armorTml tml)
    w <- genAttrFromTemplate (weaponTml tml)
    return Member { name = nm, health = h, armor = a, weapon = w }

genPartyFromTemplate :: String -> [String] -> PartyTemplate -> IO Party
genPartyFromTemplate t nms ptml = do
    ms <- zipWithM genMemberFromTemplate (nms ++ repeat noname) ptml
    return Party { title = t, members = ms }

allDead :: Party -> Bool
allDead p = all (not . isAlive) (members p)

halfRound :: Party -> Party -> IO Party
halfRound attacker defender = do
    newmbs <- attackBy getMaxHealth (filter isAlive (members attacker)) (members defender)
    return defender { members = newmbs }
    where
      attackBy _ [] dp = return dp
      attackBy s (a : as) dp = do
        dn <- attackByOne s a dp
        attackBy s as dn
      attackByOne s a dp = mapM (\d -> if d == s dp then doHit a d else return d) dp

