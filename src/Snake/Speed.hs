module Snake.Speed (Speed, maxSpeed, minSpeed, mkSpeed, toInt) where

data Speed
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

instance Bounded Speed where
  minBound = One
  maxBound = Nine

instance Enum Speed where
  fromEnum One = 1
  fromEnum Two = 2
  fromEnum Three = 3
  fromEnum Four = 4
  fromEnum Five = 5
  fromEnum Six = 6
  fromEnum Seven = 7
  fromEnum Eight = 8
  fromEnum Nine = 9

  toEnum 1 = One
  toEnum 2 = Two
  toEnum 3 = Three
  toEnum 4 = Four
  toEnum 5 = Five
  toEnum 6 = Six
  toEnum 7 = Seven
  toEnum 8 = Eight
  toEnum 9 = Nine

instance Show Speed where
  show = show . fromEnum

maxSpeed :: Speed
maxSpeed = maxBound

minSpeed :: Speed
minSpeed = minBound

mkSpeed :: Int -> Maybe Speed
mkSpeed x = if validSpeed x then Just (toEnum x) else Nothing
  where min = fromEnum minSpeed
        max = fromEnum maxSpeed
        validSpeed x = x >= min && x <= max

toInt :: Speed -> Int
toInt = fromEnum
