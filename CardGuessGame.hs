-- Author: Min Gao
{-
  I tried several methods to solve this guessing game
  and retained the most efficient method for guessing
  each number of cards (2,3,4 cards)
  But I retained all the code I wrote.
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card

--taget [Card] and guess [Card] should have same length
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback [] _ = error "The target card is an empty list"
feedback _ [] = error "The guess card is an empty list"
feedback targetCard guessCard =
    (correctCard, lowerRnak, correctRnak, higherRank, correctSuit)
    where
      correctCard = correctCardNum targetCard guessCard
      lowerRnak = lowerRankNum targetCard guessCard
      correctRnak = correctRankNum targetCard guessCard
      higherRank = higherRankNum targetCard guessCard
      correctSuit = correctSuitNum targetCard guessCard

-- the first guess
initialGuess :: Int -> ([Card], GameState)
initialGuess n
    | n < 2 = error "card number must be 2 to 4"
    | n > 4 = error "card number must be 2 to 4"
    | n == 2 = ([Card Club R5, Card Heart Jack], gs)
    | n == 3 = ([Card Club R4, Card Diamond R8, Card Spade Queen], gs)
    | n == 4 = ([Card Club R3, Card Diamond R6, Card Heart R9, Card Spade Queen], gs)
--    | n == 4 = ([Card Diamond R3, Card Heart R6, Card Spade R8, Card Club Jack], gs)
    where
      gs = GameState (buildNCardAllAnswer n allCards) 1

-- next several guess using feedback filter
nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (gc, GameState a1 t1) fb = (newgc, GameState a2 t2)
    where
      a2 = feedbackFilter a1 gc fb
      t2 = t1 + 1
--      newgc = head(a2)
      newgc = a2 !! (quot (length a2) 2)

data GameState = GameState {
    possibleAnswer :: [[Card]],
    turn :: Int
} deriving(Show)

{-  another method to solve this problem, more faster but have more guess times
-- the first guess
initialGuess :: Int -> ([Card], GameState)
initialGuess x
    | x < 2 = error "card number must be 2 to 4"
    | x > 4 = error "card number must be 2 to 4"
    | x == 2 = ([Card Club R5, Card Club Jack], gs)
    | x == 3 = ([Card Club R4, Card Club R8, Card Club Queen], gs)
    | x == 4 = ([Card Club R3, Card Club R6, Card Club R10, Card Club King], gs)
    where
      gs = GameState [] 0 allCards allRank [] 1 x (-1) [] []

nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (gc, GameState a1 c1 d1 r1 s1 t1 n1 try1 ca1 aa1) fb@(cc, lr, cr, hr, cs) =
    (newgc, GameState a2 c2 d2 r2 s2 t2 n2 try2 ca2 aa2)
    where
      a2
        | try1 /= (-1) = a1
        | (length s2) == n1 && c1 == n1 = filterCorrectAnswer a1 s2 r2
        | cc > 0 && c1 < n1 = a1 ++ gc
        | otherwise = a1
      c2
        | cc > 0 && c1 < n1 = c1 + cc
        | otherwise = c1
      d2 = filterNCard gc d2WithGuessCard
      d2WithGuessCard
        | lr == n1 = excludeHihgerRank (minRank gc) d1
        | hr == n1 = excludeLowerRank (maxRank gc) d1
        | lr + hr == n1 = excludeMiddleRank (minRank gc) (maxRank gc) d1
        | cr == n1 = excludeOtherRank gc d1
        | lr == 0 && hr == 0 = retainMiddleRank (minRank gc) (maxRank gc) d1
        | lr == 0 = retainHigherRank (minRank gc) d1
        | hr == 0 = retainLowerRank (maxRank gc) d1
        | otherwise = d1
      r2
        | cr == n1 = getRank gc
        | lr == n1 = eHRank (minRank gc) r1
        | hr == n1 = eLRank (maxRank gc) r1
        | lr + hr == n1 = eMRank (minRank gc) (maxRank gc) r1
        | lr == 0 && hr == 0 = rMRank (minRank gc) (maxRank gc) r1
        | lr == 0 = rHRank (minRank gc) r1
        | hr == 0 = rLRank (maxRank gc) r1
        | otherwise = r1
      s2
        | t1 == 1 && (length s1) /= n1 = s1 ++ (take cs (repeat Club))
        | t1 == 2 && (length s1) /= n1 && (length (getSuitCard Diamond d2)) >= n1
          = s1 ++ (take cs (repeat Diamond))
        | t1 == 3 && (length s1) /= n1 && (length (getSuitCard Heart d2)) >= n1
          = s1 ++ (take cs (repeat Heart))
        | t1 == 4 && (length s1) /= n1 && (length (getSuitCard Spade d2)) >= n1
          = s1 ++ (take cs (repeat Spade))
        | otherwise = s1
      t2 = t1 + 1
      n2 = n1
      try2
        | try1 == (-1) && c2 == n1 = 0
        | try1 /= (-1) = try1 + 1
        | otherwise = try1
      ca2
        | try2 /= (-1) && try2 /= 0 && cc == 1 = (head gc):ca1
        | otherwise = ca1
      aa2
        | n1 == 2 && t1 == 1
          = feedbackFilter (buildAnswerFTwoCard allCards) gc fb
        | n1 == 2 = feedbackFilter aa1 gc fb
      newgc
        | n1 == 2 = head aa2
        | t1 == 1 && (length s2) /= n1 && (length (getSuitCard Diamond d2)) >= n1
          = take n1 (getSuitCard Diamond d2)
        | t1 == 2 && (length s2) /= n1 && (length (getSuitCard Heart d2)) >= n1
          = take n1 (reverse(getSuitCard Heart d2))
        | t1 == 3 && (length s2) /= n1 && (length (getSuitCard Spade d2)) >= n1
          = take n1 (getSuitCard Spade d2)
        | (length ca2) == n1 = ca2
        | c2 == n1 = (a2 !! try2):(take (n1 - 1) (filterNCard a2 allCards))
        | (length d2) < n1 && (length d2) /= 0
            = d2 ++ (take (n1 - (length d2)) (filterNCard a2 allCards))
        | not(null d2) = take n1 d2
        | otherwise = take n1 a2

data GameState = GameState {
    possibleAnswer :: [Card],
    correctAnswerNum :: Int,
    possibleDeck :: [Card],
    possibleRank :: [Rank],
    correctSuit :: [Suit],
    turn :: Int,
    cardNum :: Int,
    tryAnswerNum :: Int,
    correctAnswer :: [Card],
    allpossibleAnswerFor2C :: [[Card]]  --just for two cards guess
} deriving(Show)
-}

--healper function

--all 52 cards
allCards :: [Card]
allCards = [(Card Club R2)..(Card Spade Ace)]

--all rank {2,3,4,5,6,7,8,9,T,J,Q,K,A}
allRank :: [Rank]
allRank = [minBound..maxBound] :: [Rank]

--all suit {Club: C, Diamond: D, Heat: H, Spade: S}
allSuit :: [Suit]
allSuit = [minBound..maxBound] :: [Suit]

--the number of correct cards
correctCardNum :: [Card] -> [Card] -> Int
correctCardNum [] [] = 0
correctCardNum _ [] = 0
correctCardNum [] _ = 0
correctCardNum (c1:cs1) cs2
    | oneCardMatch c1 cs2 = 1 + correctCardNum cs1 (filterOneCard c1 cs2)
    | otherwise = 0 + correctCardNum cs1 cs2

--the number of cards in answer have rank lower than the lowest rank in guess
lowerRankNum :: [Card] -> [Card] -> Int
lowerRankNum [] [] = 0
lowerRankNum _ [] = 0
lowerRankNum [] _ = 0
lowerRankNum ((Card s1 r1):cs1) cs2
    | r1 < minRank(cs2) = 1 + lowerRankNum cs1 cs2
    | otherwise = 0 + lowerRankNum cs1 cs2

--the number of correct rank
correctRankNum :: [Card] -> [Card] -> Int
correctRankNum [] [] = 0
correctRankNum _ [] = 0
correctRankNum [] _ =0
correctRankNum ((Card s1 r1):cs1) cs2
    | oneRankMatch (Card s1 r1) cs2 = 1 + correctRankNum cs1 (filterOneRank r1 cs2)
    | otherwise = 0 + correctRankNum cs1 cs2

--the number of cards in answer have rank higher than the highest rank in the guess
higherRankNum :: [Card] -> [Card] -> Int
higherRankNum [] [] = 0
higherRankNum _ [] = 0
higherRankNum [] _ = 0
higherRankNum ((Card s1 r1):cs1) cs2
    | r1 > maxRank(cs2) = 1 + higherRankNum cs1 cs2
    | otherwise = 0 + higherRankNum cs1 cs2

--the number of correct suit (only counting a card in the guess once)
correctSuitNum :: [Card] -> [Card] -> Int
correctSuitNum [] [] = 0
correctSuitNum _ [] = 0
correctSuitNum [] _ = 0
correctSuitNum ((Card s1 r1):cs1) cs2
    | oneSuitMatch (Card s1 r1) cs2 = 1 + correctSuitNum cs1 (filterOneSuit s1 cs2)
    | otherwise = 0 + correctSuitNum cs1 cs2

--is there a card suit in a deck
oneSuitMatch :: Card -> [Card] -> Bool
oneSuitMatch _ [] = False
oneSuitMatch c1@(Card s1 r1) ((Card s2 r2):cs2)
    | s1 == s2 = True
    | cs2 == [] = False
    | otherwise = oneSuitMatch c1 cs2

--is there a card rank in a deck
oneRankMatch :: Card -> [Card] -> Bool
oneRankMatch _ [] = False
oneRankMatch c1@(Card s1 r1) ((Card s2 r2):cs2)
    | r1 == r2 = True
    | cs2 == [] = False
    | otherwise = oneRankMatch c1 cs2

--is there a card in a deck
oneCardMatch :: Card -> [Card] -> Bool
oneCardMatch _ [] = False
oneCardMatch c1 (c2:cs2)
    | c1 == c2 = True
    | cs2 == [] = False
    | otherwise = oneCardMatch c1 cs2

--filter all the same Card in one card deck
filterOneCard' :: Card -> [Card] -> [Card]
filterOneCard' c1 cs2 = [cs | cs <- cs2, cs /= c1]

--can have two same card in one card deck, filter one card from a deck
filterOneCard :: Card -> [Card] -> [Card]
filterOneCard _ [] = []
filterOneCard c1 (c2:cs2)
    | c1 == c2 = cs2
    | otherwise = c2:(filterOneCard c1 cs2)

--filter several card in one deck, use for other method
filterNCard :: [Card] -> [Card] -> [Card]
filterNCard [] [] = []
filterNCard [] ys = ys
filterNCard _ [] = []
filterNCard (c1:cs1) cs2
    | c1 `elem` cs2 =filterNCard cs1 (filterOneCard c1 cs2)
    | otherwise = filterNCard cs1 cs2

--filter a card of one rank in a deck
filterOneRank :: Rank -> [Card] -> [Card]
filterOneRank _ [] = []
filterOneRank r1 ((Card s2 r2):cs2)
    | r1 == r2 = cs2
    | otherwise = (Card s2 r2):(filterOneRank r1 cs2)

--filter a card of one suit in a deck
filterOneSuit :: Suit -> [Card] -> [Card]
filterOneSuit _ [] = []
filterOneSuit s1 ((Card s2 r2):cs2)
    | s1 == s2 = cs2
    | otherwise = (Card s2 r2):(filterOneSuit s1 cs2)

--the minimun rank in a deck
minRank :: [Card] -> Rank
minRank [] = error "minRank of empty list"
minRank [Card s r] = r
minRank ((Card s r):cs) = min r (minRank cs)

--the maximun rank in a deck
maxRank :: [Card] -> Rank
maxRank [] = error "maxRank of empty list"
maxRank [Card s r] = r
maxRank ((Card s r):cs) = max r (maxRank cs)

--filter the possible answer through the feedback
feedbackFilter :: [[Card]] -> [Card] -> (Int, Int, Int, Int, Int) -> [[Card]]
feedbackFilter [] _ _ = []
feedbackFilter (cs:css) gc fb
    | (feedback cs gc) == fb = cs:feedbackFilter css gc fb
    | otherwise = feedbackFilter css gc fb

--build all possible answer for n cards guess
buildNCardAllAnswer :: Int -> [Card] -> [[Card]]
buildNCardAllAnswer 0 _ = [[]]
buildNCardAllAnswer _ [] = []
buildNCardAllAnswer n (c:cs) =
    (map ([c]++) (buildNCardAllAnswer (n - 1) cs)) ++ (buildNCardAllAnswer n cs)

{- some old version function, or use for other methods
--exclude the higher rank in the card deck
excludeHihgerRank :: Rank -> [Card] -> [Card]
excludeHihgerRank _ [] = []
excludeHihgerRank x ((Card s r):cs)
    | r < x = (Card s r):excludeHihgerRank x cs
    | otherwise = excludeHihgerRank x cs

--exclude the higher rank in the rank deck
eHRank :: Rank -> [Rank] -> [Rank]
eHRank _ [] = []
eHRank x (r:rs)
    | r < x = r:eHRank x rs
    | otherwise = eHRank x rs

--exclude the lower rank in the card deck
excludeLowerRank :: Rank -> [Card] -> [Card]
excludeLowerRank _ [] = []
excludeLowerRank x ((Card s r):cs)
    | r > x = (Card s r):excludeLowerRank x cs
    | otherwise = excludeLowerRank x cs

--exclude the lower rank in the rank deck
eLRank :: Rank -> [Rank] -> [Rank]
eLRank _ [] = []
eLRank x (r:rs)
    | r > x = r:eLRank x rs
    | otherwise = eLRank x rs

--exclude the middle rank between two rank in the card deck
excludeMiddleRank :: Rank -> Rank -> [Card] -> [Card]
excludeMiddleRank _ _ [] = []
excludeMiddleRank r1 r2 ((Card s r):cs)
    | r1 > r || r > r2 = (Card s r):excludeMiddleRank r1 r2 cs
    | otherwise = excludeMiddleRank r1 r2 cs

--exclude the middle rank between two rank in the rank deck
eMRank :: Rank -> Rank -> [Rank] -> [Rank]
eMRank _ _ [] = []
eMRank r1 r2 (r:rs)
    | r1 > r || r > r2 = r:eMRank r1 r2 rs
    | otherwise = eMRank r1 r2 rs

--exclude the other rank card in the card deck
excludeOtherRank :: [Card] -> [Card] -> [Card]
excludeOtherRank _ [] = []
excludeOtherRank [] ys = ys
excludeOtherRank cs1 (c2:cs2)
    | oneRankMatch c2 cs1 = c2:excludeOtherRank cs1 cs2
    | otherwise = excludeOtherRank cs1 cs2

--retain higher rank in the rank deck
retainHigherRank :: Rank -> [Card] -> [Card]
retainHigherRank _ [] = []
retainHigherRank x ((Card s r):cs)
    | r >= x = (Card s r):retainHigherRank x cs
    | otherwise = retainHigherRank x cs

--retain higher rank in the rank deck
rHRank :: Rank -> [Rank] -> [Rank]
rHRank _ [] = []
rHRank x (r:rs)
    | r >= x = r:rHRank x rs
    | otherwise = rHRank x rs

--retain lower rank in the card deck
retainLowerRank :: Rank -> [Card] ->[Card]
retainLowerRank _ [] = []
retainLowerRank x ((Card s r):cs)
    | r <= x = (Card s r):retainLowerRank x cs
    | otherwise = retainLowerRank x cs

--retain lower rank in the rank deck
rLRank :: Rank -> [Rank] -> [Rank]
rLRank _ [] = []
rLRank x (r:rs)
    | r <= x = r:rLRank x rs
    | otherwise = rLRank x rs

--retain middle rank between two rank in the card deck
retainMiddleRank :: Rank -> Rank -> [Card] -> [Card]
retainMiddleRank _ _ [] = []
retainMiddleRank r1 r2 ((Card s r):cs)
    | r1 <= r && r <= r2 = (Card s r):retainMiddleRank r1 r2 cs
    | otherwise = retainMiddleRank r1 r2 cs

--retain middle rank between two rank in the rank deck
rMRank :: Rank -> Rank -> [Rank] -> [Rank]
rMRank _ _ [] = []
rMRank r1 r2 (r:rs)
    | r1 <= r && r <= r2 = r:rMRank r1 r2 rs
    | otherwise = rMRank r1 r2 rs

--the same as getRank
getRank' :: [Card] -> [Rank]
getRank' [] = []
getRank' cs = map rank cs

--get a Cards' list of rank
getRank :: [Card] -> [Rank]
getRank [] = []
getRank ((Card s r):cs) = r:getRank cs

--get a Cards' list of suit
getSuitCard :: Suit -> [Card] -> [Card]
getSuitCard _ [] = []
getSuitCard s1 ((Card s r):cs)
    | s == s1 = (Card s r):getSuitCard s1 cs
    | otherwise = getSuitCard s1 cs

--only retain the specific suit and rank in a [card]
filterCorrectAnswer :: [Card] -> [Suit] -> [Rank] -> [Card]
filterCorrectAnswer [] _ _ = []
filterCorrectAnswer ((Card s r):cs) ss rr
    | s `elem` ss && r `elem` rr = (Card s r):filterCorrectAnswer cs ss rr
    | otherwise = filterCorrectAnswer cs ss rr

-- build all possible answer for two cards guess
buildAnswerFTwoCard :: [Card] -> [[Card]]
buildAnswerFTwoCard [] = []
buildAnswerFTwoCard (c:cs) = (buildOne c cs) ++ buildAnswerFTwoCard cs

buildOne :: Card -> [Card] -> [[Card]]
buildOne _ [] = []
buildOne c1 cs = [c1:[head cs]] ++ (buildOne c1 (drop 1 cs))

buildAnswer'' :: [Card] -> [[Card]]
buildAnswer'' [] = []
buildAnswer'' (c:cs) = (buildOne c cs) ++ buildAnswer'' cs

-- this method is too slow, waste time
buildAnswer :: Int -> [Card] -> [[Card]]
buildAnswer _ [] = []
buildAnswer n cs
    | n == 2 = filterSameAnswer (sortSameAnswer [[x, y] | x <- cs, y <- cs, x /= y])
    | n == 3 = filterSameAnswer (sortSameAnswer [[x, y, z] |
               x <- cs, y <- cs, z <- cs, x /= y && y /= z && x /= z])
    | n == 4 = filterSameAnswer (sortSameAnswer [[w, x, y, z] | w <- cs ,x <- cs,
               y <- cs, z <- cs, w /= x && w /= y && w /= z && x /= y && x /= z && y /= z])
    | otherwise = error "n can only be 2,3,4"

-- this method will waste time
filterSameAnswer :: [[Card]] -> [[Card]]
filterSameAnswer [] = []
filterSameAnswer (cs:css)
    | cs `elem` css = filterSameAnswer css
    | otherwise = cs:filterSameAnswer css

-- this sort function needs to import Data.List
sortSameAnswer :: [[Card]] -> [[Card]]
sortSameAnswer [] = []
sortSameAnswer (cs:css) = (sort cs):sortSameAnswer css

-}
