import System.Random  -- give us a way to generate random numbers!

data CardNumber = Two   | Three | Four | Five | Six
                | Seven | Eight | Nine | Ten  | Knight 
                | Queen | King  | Ace
                deriving (Bounded, Enum, Eq)

data CardSuit   = Club | Heart | Diamond | Spade deriving (Bounded, Enum, Eq)

data Card = Card CardNumber CardSuit deriving (Eq)

type Deck = [Card]

instance Show CardNumber where
  show Two    = "2"
  show Three  = "3"
  show Four   = "4"
  show Five   = "5"
  show Six    = "6"
  show Seven  = "7"
  show Eight  = "8"
  show Nine   = "9"
  show Ten    = "10"
  show Knight = "Kn"
  show Queen  = "Q"
  show King   = "K"
  show Ace    = "A"

instance Show CardSuit where
  show Club    = "♣"
  show Heart   = "♥"
  show Diamond = "♦"
  show Spade   = "♠"

instance Show Card where
  show (Card n s) = (show n) ++ (show s)

{--
draw :: Deck -> (Card, Deck)
draw [] = undefined
draw (x:xs) = (x, xs)
--}

draw :: Deck -> (Card, Deck)
draw xs = (head $ fst l, snd l)
  where l = drawN 1 xs

drawN :: Int -> Deck -> (Deck, Deck)
drawN _ [] = undefined
drawN n xs
    | n > length xs = undefined
    | otherwise     = (take n xs, drop n xs)

countDeck :: Deck -> Int
countDeck cards = length cards

deck :: Deck
deck = reverse $ deck' [Card Two Club]

deck' :: Deck -> Deck
deck' (f@(Card Ace Spade):l) = f : l
deck' (f@(Card  cn Spade):l) = deck' (Card (succ cn)     Club  : f : l)
deck' (f@(Card  cn    ct):l) = deck' (Card       cn  (succ ct) : f : l)

main = do
          print deck
          print $ length deck 
          print (Card Two Club == Card Two Heart)
          print (Card Two Club == Card Two Heart)
          print $ countDeck [Card Two Club, Card Two Heart]
          print $ draw deck
          print $ drawN 10 (deck)



-- Some notes about design
{-
We will treat a deck of cards as a simple haskell list of cards.

We are going to consider the "top" of the deck to be the front of a singly linked list
That is top (x:xs) = x

That  means the bottom of the deck will be the last element of the list. In haskell terms
we will call the bottom of the deck the same as bottom xs = last xs

-}

-- read "2" -> Two
-- read "H" -> Heart
-- read "♥" -> Heart
-- read "2H" -> Card Two Heart