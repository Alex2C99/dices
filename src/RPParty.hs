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
    weaponTml :: AttribTemplate,
    strategyTml :: Strategy
  }

data Member = Member {
    name   :: String,
    health :: Int,
    armor  :: Int,
    weapon :: Int,
    strategy :: Strategy
  }

data Party = Party {
    title   :: String,
    members :: [Member]
  }

type PartyTemplate = [MemberTemplate]

type Strategy = Member -> [Member] -> IO [Member]

instance Eq Member where
  (==) m1 m2 = (name m1 == name m2) 
    && (health m1 == health m2) 
    && (armor m1 == armor m2) 
    && (weapon m1 == weapon m2) 

noname :: String
noname = "NoName"

isAlive :: Member -> Bool
isAlive mb = health mb > 0

isHit :: Member -> IO Bool
isHit mb = fmap ((> armor mb) . sumd) (cast One 20)

getDamage :: Member -> Int -> Member
getDamage mb dmg
    | isAlive mb = mb { health = if health mb - dmg > 0 then health mb - dmg else 0 }
    | otherwise  = mb

doHit :: Member -> Member -> IO Member
doHit attacker target = do
   logBegin attacker target
   isHit target >>= execHit attacker target
   where
     execHit a t True = do
        (Single dmg) <- cast One (weapon a)
        logResult True dmg
        let nt = getDamage t dmg
        logFinish nt
        return nt
     execHit _ t False = do
        logResult False (0 :: Int)
        return t
     logBegin a t = putStr $ "\t" ++ name attacker ++ " attacks " ++ name target ++ ". "
     logResult True v  = putStr $ "Hit! Damage: " ++ show v ++ "\n"
     logResult False _ = putStr "Miss!\n"
     logFinish t = when (health t == 0) $ putStrLn $ name t ++ " killed!\n"

getMaxHealth :: [Member] -> Member
getMaxHealth = maximumBy (\a b -> compare (health a) (health b))

getMinHealth :: [Member] -> Member
getMinHealth = minimumBy (\a b -> compare (health a) (health b))

genAttrFromTemplate :: AttribTemplate -> IO Int
genAttrFromTemplate atml = fmap ((start atml +) . sumd) (cast (throw atml) (top atml))

genMemberFromTemplate :: String -> MemberTemplate -> IO Member
genMemberFromTemplate nm tml = do
    h <- genAttrFromTemplate (healthTml tml)
    a <- genAttrFromTemplate (armorTml tml)
    w <- genAttrFromTemplate (weaponTml tml)
    return Member { name = nm, health = h, armor = a, weapon = w, strategy = strategyTml tml }

genPartyFromTemplate :: String -> [String] -> PartyTemplate -> IO Party
genPartyFromTemplate t nms ptml = do
    ms <- zipWithM genMemberFromTemplate (nms ++ repeat noname) ptml
    return Party { title = t, members = ms }

allDead :: Party -> Bool
allDead p = all (not . isAlive) (members p)

hitOneStrategy :: ([Member] -> Member) -> Strategy
hitOneStrategy f am defms = mapM hitOne defms
  where 
  hitOne d
    | d == f defms = doHit am d
    | otherwise    = return d

hitAllStrategy :: Strategy
hitAllStrategy am = mapM (doHit am)

maxHealthStrategy :: Strategy
maxHealthStrategy = hitOneStrategy getMaxHealth

minHealthStrategy :: Strategy
minHealthStrategy = hitOneStrategy getMinHealth 

halfRound :: Party -> Party -> IO Party
halfRound attacker defender = do
    putStrLn $ "\nParty " ++ title attacker ++ " attacks:"
    newmbs <- foldM (\dp a -> strategy a a dp) (alives (members defender)) (alives (members attacker))
    return defender { members = newmbs }
    where 
      alives = filter isAlive

fighterTemlate = MTml {
  healthTml = ATml { throw = One, start = 10, top = 20 },
  weaponTml = ATml { throw = One, start = 0, top = 9 },
  armorTml = ATml  { throw = One, start = 0, top = 15 },
  strategyTml = maxHealthStrategy
}
                 