-- Name: Alfons Fernaldy
-- Student ID: 30127831
-- Programming Paradigms Assignment 2
-- ================================================================================================================= --
--                                    Overall AI Strategy & Design Choices                                           --                                                             --
-- ================================================================================================================= --
{-
There are 2 variations of a Hearts game which are as follows:
    i.) 1 versus 1, where each player receives 26 cards
    ii.) 4 versus 4, where each player receives 13 cards

Furthermore each variant has 2 player types:
    i.) Non SQ Holder who don't have the Queen of spades in hand
    ii.) SQ Holder who has the Queen of spades in hand

I have created two AIs in the form of a seperate lead/renege methods in order to handle each variant.
Each Variant will also have its own sub divergences for players holding the Queen of Spades as well as those who don't.
The reason I did not use only one AI (lead/renege) was because it would be very inefficient to create
one general AI to handle both variants of the Hearts game which clearly have differing strategies to win
due to factors like number of players, card distribution, etc...

VARIANT I: 4v4 Battle Royale [Implemented in lead & renege]

    (A.) Non SQ Holder Strategy: Early Aggressive + Mid-Late game Conservative
    A Non SQ Holder must do the following in order to survive the trick with as few point cards as possible:
        1. Avoid leading (except for the first few rounds)
        2. Getting rid of high (risky) cards whenever the opportunity arises
        3. Conserving low (safe) cards to stay out of leading
    Ideally by middle to late game, a Non SQ holder should have low cards to stay out of leading and avoid getting point cards
 
    (B.) SQ Holder Strategy: Void Club/Diamond suit ASAP + Conservative after SQ thrown away 
    An SQ Holder must do the following in order to throw away his/her Queen of Spades as well as to survive the trick with few point cards:
        1. Void either Diamonds or Clubs as soon as possible
        2. Keep playing high cards during renege to stay in leading
        3. Being in the lead allows the SQ holder to easily get rid of Diamond/Club (whichever is the fewest)
        4. Throw away Queen of spades ASAP and then play conservatively to avoid accumulating Heart cards
    Ideally by middle game, a SQ hodler would have depleted Diamond/Club suites and be left with low cards enough to get out of lead
    After this simply stay out of lead until another player leads with a card whose suit we have already depleted allowing us to throw SQ.
    By playing aggressively early on we also make it easier to stay out of lead after throwing SQ away to avoid hearts.

VARIANT II: 1v1 Match [Implemented in lead2 & renege2]

    (A.) Non SQ Holder & SQ Holder Strategy: Early Aggressive + Mid-Late game Conservative + Sacrifice Highest Heart
    A Non SQ Holder must do the following in order to survive the trick with as few point cards as possible:
        1. Avoid leading (except for the first few rounds)
        2. Getting rid of high (risky) cards whenever the opportunity arises
        3. Conserving low (safe) cards to stay out of leading
        4. Conserving the Highest heart to prevent a "shoot the moon" attempt
    Ideally by middle to late game, a Non SQ Holder would have gotten rid of the larger cards and can safely stay in renege with low cards.
    Also by conserving the highest heart, we can prevent the opponent from accumulating all hearts by purposely winning a trick 
    in the last few rounds ideally the last round so the opponent would not be able to capitalize on the next round lead.

NOTE: I found that including a seperate strategy for SQ & Non SQ holders did not improve performance so I have chosen to use a general strategy.
-}
module Player (
    playCard,
    makeBid,
    lead,
    renege
)
where

import Cards
import Data.Maybe
import Data.List
import Hearts.Types

playCard :: PlayFunc
-- PlayFunc passes in these 4 variables   
-- PlayerId -- this player's Id so they can identify themselves in the bids and tricks
-- [Card]   -- the player's cards
-- [(Card, PlayerId)]    -- cards in the current trick, so far
-- Maybe ([(Card, PlayerId)], String) -- previous player's state

-- Using the above, we are able to determine the variant of the game (1v1 or 4v4) and subsequently use the 
-- appropriate lead/renege function pairs
-- Memory is also updated in playCard using the previous player's state and it's passed into lead & renege to be used
-- More details about memory can be found in the MEMORY section below

-- Current trick list is empty indicating I must lead
playCard _ hand [] previous
    | isTwoPlayerGame hand previous = (lead2 hand (getMemoryList previous), (writeMemory (getMemoryString previous) (getPreviousTrick previous)))
    | otherwise = (lead hand (getMemoryList previous), (writeMemory (getMemoryString previous) (getPreviousTrick previous)))
    where
        -- Check either number of cards in the hand (first round only) or 
        -- length of trick in previous player's state to determine if the current game is a 1v1 or 4v4
        isTwoPlayerGame :: [Card] -> Maybe ([(Card, PlayerId)], String) -> Bool
        isTwoPlayerGame handList Nothing =  length handList == 26
        isTwoPlayerGame _ (Just (list, _)) = length list == 2
        
-- Trick list is not empty indicating someone has already lead therefore I must renege
playCard _ hand trick previous
    | isTwoPlayerGame hand previous = (renege2 trick hand (getMemoryList previous), (writeMemory (getMemoryString previous) (getPreviousTrick previous)))
    | otherwise = (renege trick hand (getMemoryList previous), (writeMemory (getMemoryString previous) (getPreviousTrick previous)))
    where
        isTwoPlayerGame :: [Card] -> Maybe ([(Card, PlayerId)], String) -> Bool
        isTwoPlayerGame handList Nothing =  length handList == 26
        isTwoPlayerGame _ (Just (list, _)) = length list == 2

-- ================================================================================================================= --
--                                        MEMORY READ & WRITE FUNCTIONS                                              --                                                             --
-- ================================================================================================================= --
{-
For memory, I have decided to store it as a simplified string format demonstrated below:

Initial: ""
Trick #1 [Card Club Two, Card Club Four, Card Club Six, Card Club Seven]
Memory: "C2 C4 C6 C7"
Trick #2 [Card Diamond Ten, Card Diamond Eight, Card Diamond Queen, Card Diamond Four]
Updated memory: "C2 C4 C6 C7 D10 D8 DQ D4"

I decided to choose this format because there was no need to keep track of the respective PlayerId of each card
as it is irrelevent in my current strategy. Furthermore this format is very simple to update & read as it can be
converted to/from tuple->list->string and string->list very easily as shown below
-}

-- memoryStringToList converts a string representation of cards into a list of cards
-- String: "S2 S3 S7 S9" -> [Card]: [S2, S3, S7, S9]
-- words "S2 S3 S7 S9" -> ["S2", "S3", "S7", "S9"]
-- Map every string in the above list with (read a :: Card) which converts a string "C2" into respective Card type C2
memoryStringToList :: String -> [Card]
memoryStringToList str =  map (\a -> (read a :: Card)) (words str)

-- This is just the sequence of prevStateToString -> memoryStringToList
getPreviousTrick :: Maybe ([(Card, PlayerId)], String) -> [Card]
getPreviousTrick = memoryStringToList . prevStateToString
    where
        -- prevStateToString extracts the tuple of cards from the previous trick and converts it into a string representation
        -- Maybe ([(Card, PlayerId)], String): 
        --(Just ([(Card Club Two, "1"), (Card Heart Three, "2"), (Card Spade Seven, "3"), (Card Diamond Nine, "4")], ""))
        -- String: "S2 S3 S7 S9" which can be concatenated to memory string
        -- Use fmap twice to convert  [(Card Club Two, "1"), (Card Heart Three, "2"), (Card Spade Seven, "3"), (Card Diamond Nine, "4")]
        --      into ["S2", "S3", "S7", "S9"]
        -- Use intercalate to format this list as "S2 S3 S7 S9"
        prevStateToString :: Maybe ([(Card, PlayerId)], String) -> String
        prevStateToString Nothing = []
        prevStateToString (Just (list, _)) = intercalate " " (show <$> fst <$> list)

-- Extracts only the String in a Maybe ([(Card, PlayerId)], String)
-- Used to retrieve the memory as string in order to update the memory by concatenating previous state cards as string
getMemoryString :: Maybe ([(Card, PlayerId)], String) -> String
getMemoryString Nothing = ""
getMemoryString (Just (_, str)) = str

-- This is just the sequence of getMemoryString -> memoryStringToList
-- Convenience function reformat the previous state tuple directly into a usable list of cards
getMemoryList :: Maybe ([(Card, PlayerId)], String) -> [Card]
getMemoryList = memoryStringToList. getMemoryString

-- Properly concatenates the memory as a string & the list of cards of the previous state
writeMemory :: String -> [Card] -> String
writeMemory "" list = cardListToString list
writeMemory memory list = memory ++ " " ++ (cardListToString list)

-- Converts a card list to string, used when writing previous state's cards into memory as a string
cardListToString :: [Card] -> String
cardListToString list = intercalate " " (map (\a -> (show a)) list)

-- ================================================================================================================= --
--                                        PATTERN MATCHING FUNCTIONS                                                 --                                                             --
-- ================================================================================================================= --

{- 
Below are the reusable "filter" functions which will be an integral part in implementing the strategies
highlighted in the top section. More specific filter functions which are only used once will be
included inside "where" of their respective lead/renege functions.
-}

----- {CARD SPECIFIC} ----- 

-- Filters for Two of Clubs
fTwoClubs :: [Card] -> [Card]
fTwoClubs = filter (\x -> rank x == Two) . fClubs

-- Filters for Queen of Spades
fSQ :: [Card] -> [Card]
fSQ = filter (\x -> rank x == Queen) . fSpades

-- Filters for Cards which is not Queen of Spades
fNotSQ :: [Card] -> [Card]
fNotSQ hand
    | fSQ hand /= [] = filter (\x -> x /= Card Spade Queen) hand
    | otherwise = hand

----- {DIAMONDS & CLUBS} -----
-- Filters for Diamond suit cards
fDiamonds :: [Card] -> [Card]
fDiamonds = filter (\x -> suit x == Diamond)

-- Filters for Diamond suit cards which have a rank above or equivalent to Jack
fRiskyDiamonds :: [Card] -> [Card]
fRiskyDiamonds hand = filter (\x -> rank x >= Jack) (fDiamonds hand)

-- Filters for Club suit cards
fClubs :: [Card] -> [Card]
fClubs = filter (\x -> suit x == Club)

-- Filters for Club suit cards which have a rank above or equivalent to Jack
fRiskyClubs :: [Card] -> [Card]
fRiskyClubs hand = filter (\x -> rank x >= Jack) (fClubs hand)

-- Convenient function used to combine Club & Diamond cards with rank above/equal to Jack
fRiskyDC :: [Card] -> [Card]
fRiskyDC hand = fRiskyDiamonds hand ++ fRiskyClubs hand

----- { HEARTS } -----
-- Filters for Heart suit cards
fHearts :: [Card] -> [Card]
fHearts = filter (\x -> suit x == Heart)

-- Filters for Heart suit cards but exclude the highest rank heart card
fHeartsNoHighest :: [Card] -> [Card]
fHeartsNoHighest = fHearts . fNotHighestHeart

-- Filters for cards that are not the highest rank heart card
fNotHighestHeart :: [Card] -> [Card]
fNotHighestHeart hand
    | fHearts hand /= [] = filter (\x -> x /= fHighestCard Heart hand) hand
    | otherwise = hand

----- { SPADE } -----
-- Filters for Spade suit cards
fSpades :: [Card] -> [Card]
fSpades = filter (\x -> suit x == Spade)

-- Filters for Spade suit cards with rank above Queen
fRiskySpades :: [Card] -> [Card]
fRiskySpades = filter (\x -> rank x > Queen) . fSpades

-- Filters for Spade suit cards with rank below Queen
fSafeSpades :: [Card] -> [Card]
fSafeSpades = filter (\x -> rank x < Queen) . fSpades

-- Filters for Spade suit cards but exclude the Queen of Spades
fSpadesNotSQ :: [Card] -> [Card]
fSpadesNotSQ = filter (\x -> rank x /= Queen) . fSpades

-- Returns a list of cards of the same suit as leadSuit AND lower rank than highest card in trick
fSuit :: Suit -> [Card] -> [Card]
fSuit leadSuit = filter (\x -> suit x == leadSuit)

-- Returns the highest ranked card of targetSuit
fHighestCard :: Suit -> [Card] -> Card
fHighestCard targetSuit hand = head $ sortDesc $ fSuit targetSuit hand

-- Sorts list of card by their rank in ascending order disregarding their Suit
fSortByRank :: [Card] -> [Card]
fSortByRank hand = filter(\x -> rank x == Two) hand ++ filter(\x -> rank x == Three) hand ++ filter(\x -> rank x == Four) hand ++ filter(\x -> rank x == Five) hand ++ filter(\x -> rank x == Six) hand ++ filter(\x -> rank x == Seven) hand ++
               filter(\x -> rank x == Eight) hand ++ filter(\x -> rank x == Nine) hand ++ filter(\x -> rank x == Ten) hand ++ filter(\x -> rank x == Jack) hand ++ filter(\x -> rank x == Queen) hand ++ filter(\x -> rank x == King) hand ++ filter(\x -> rank x == Ace) hand

-- Sorts list of card by their rank in descending order disregarding their Suit
fSortByRankDesc :: [Card] -> [Card]
fSortByRankDesc = reverse . fSortByRank

-- ================================================================================================================= --
--                                                LEAD FUNCTION                                                      --
--                   The lead function for a traditional 4v4 Hearts Battle Royale with 13 tricks                     --                                                              --
-- ================================================================================================================= --
{-How it works:
    Arrange the cards in a hand based on a pre-determined "priority list".
    The card to be played at any round will be the first card in this list.
    ----------[LEAD PRIORITY LIST]----------
    [A] Non SQ Holder
        1.) Two of Clubs ---> fTwoClubs
            Reason: Game rules state that Two of Clubs must be played first
        2.) (Early Game first 3 rounds) Risky Club/Diamond ---> fEarlyRiskyLead
            Reason: First 3 rounds of a 4v4 game is usually the best time to throw away high cards because
            there is a low chance that hearts/SQ is played this early on. Therefore it is worth the risk.
        3.) Lowest Club, Diamond, Spade (also Heart if hearts broken) ---> fSafeLead
            Reason: After early game, try to get out of lead by playing the lowest rank card available.
            Make sure to play lowest heart after hearts has been broken. Priority should look like this [D2, C2, S3, H4, ...]
        4.) Risky Spades (Spades > Queen) ---> fRiskySpades
            Reason: Playing Risky Spades in lead is NOT ideal and should be avoided therefore 
            I am putting it low on the priority list to comply with game rules (must play all available cards)
        5.) Queen of Spades ---> fSQ
            Reason: SQ is the worst card to play in lead unless you are trying to shoot the moon

    [B] SQ Holder
        1.) Two of Clubs ---> fTwoClubs
        2.) (Diamond & Club suit NOT depleted) Highest/Diamond Club  ---> fDepleteDC
            Reason: Focus on voiding either Diamond/Club to get rid of SQ in renege round of depleted suit.
            It should be safe to stay in leading until the mid game as we hold the Queen of Spades.
        4.) Lowest Club, Diamond (also Heart if hearts broken) ---> fSafeLeadDCH
            Reason: After depleting either Diamond/Club cards, get out of leading ASAP and hope 
            another player will lead with the depleted Suit in the following rounds so that we can get rid of SQ.
            Priority should look like this: [D2, C2, S3, H4, ...]
        5.) Lowest Safe Spades (Spades < Queen) ---> (sort $ fSpades hand)
            Reason: I put spades as a lower priority because they should be reserved for renege to survive attempts
            to bleed spades (other player leading with spades in an attempt to force SQ holder to play SQ)
        6.) Queen of Spades
        Reason: SQ is the worst card to play in lead unless you are trying to shoot the moon

-}
lead :: [Card] -> [Card] -> Card
lead hand memory 
-- SQ Holder
    | isHoldingSQ hand =
        select $ (fTwoClubs hand ++ 
        fDepleteDC hand++
        fSafeLeadDCH hand memory ++
        (sort $ fSpades hand) ++
        fSQ hand)

-- Non SQ Holder
    | otherwise =
        select $ (fTwoClubs hand ++ 
        fEarlyRiskyLead hand memory ++
        fSafeLead hand memory ++
        fRiskySpades hand ++
        fSQ hand)

    where
    -- | Select the first card from hand if no particular card was provided.
    select :: [Card] -> Card
    select [] = head hand
    select x = head x

    ----------[Non SQ Holder Functions]----------
    -- Filters for Risky Diamonds&Clubs only if it is the first 3 tricks of a round, [] otherwise
    fEarlyRiskyLead :: [Card] -> [Card] -> [Card]
    fEarlyRiskyLead handList memoryList
        | (length memoryList <= 4) = fRiskyDC handList
        | otherwise = []
    
    -- Filters for the lowest rank card possible, if hearts broken then include lowest hearts as well
    fSafeLead :: [Card] -> [Card] -> [Card]
    fSafeLead handList memoryList
        | (heartsBroken memoryList) = fSortByRank $ fDiamonds handList ++ fClubs handList ++ fHearts handList ++ fSafeSpades handList
        | otherwise = fSortByRank $ fDiamonds handList ++ fClubs handList ++ fSafeSpades handList

    ----------[SQ Holder Functions]----------
    -- Filters for the lowest rank Diamond, Club and Heart (if hearts broken) cards
    fSafeLeadDCH :: [Card] -> [Card] -> [Card]
    fSafeLeadDCH handList memoryList = fSpadesNotSQ $ fSafeLead handList memoryList

    -- Filters for the highest Diamond/Club cards based on whichever suit has the least cards
    -- Return [] if either one/both of the suit are void.
    fDepleteDC :: [Card] -> [Card]
    fDepleteDC handList
        | (count handList Club == 0 || count hand Diamond == 0) = []
        | (count handList Club <= count hand Diamond) = sortDesc $ fClubs handList
        | otherwise = sortDesc $ fDiamonds hand

-- ================================================================================================================= --
--                                                LEAD2 FUNCTION                                                     --
--                            This lead function for a 1v1 Hearts game with 26 tricks                                --                                                              --
-- ================================================================================================================= --
{-How it works:
    Arrange the cards in a hand based on a pre-determined "priority list".
    The card to be played at any round will be the first card in this list.
    ----------[LEAD PRIORITY LIST]----------
    [A] Conseravative + Anti Shoot the Moon
        1.) Two of Clubs
        2.) (Early Game first 8 rounds) Highest Club/Diamond
            Reason: Unlike a 4v4 game, a 1v1 has more breathing space when it comes to throwing away large cards.
            Due to the large hand, it is unlikely for a player to break hearts/throw SQ early on making the first 8 rounds
            the best time to get rid of the higher cards.
        3.) Lowest Club, Diamond, Safe Spade (also Hearts if hearts broken EXCEPT HIGHEST HEART)
            Reason: After early game, try to get out of lead by playing lowest non point card possible
            Never give away the highest heart card, keep this until the very end and hope to lure the other player's high heart cards
        4.) Risky Spades (Spades > Queen)
            Reason: Last resort lead, risky spades should be played in a non-match renege
        5.) Queen of Spades
            Reason: SQ should NOT be played on Lead as it would lead to the leader getting it.
        6.) Last Highest Heart
            Reason: This is the core strategy to counter shoot the moon players. By witholding the highest heart card,
            I can bait the opponent to collect all the point cards only for me to purposely win one trick and prevent
            the opponent from shooting the moon.

-}
lead2 :: [Card] -> [Card] -> Card
lead2 hand memory =
    select $ (fTwoClubs hand ++
    fEarlyHighLead hand memory ++
    fSafeLead2 hand memory ++
    fRiskySpades hand ++
    fSQ hand ++
    fHearts hand ++
    sort hand)
    where
        select :: [Card] -> Card
        select [] = head hand
        select x = head x
        -- Similar to fEarlyRiskyLead in lead function above but this applies for 8 rounds instead of 3
        fEarlyHighLead :: [Card] -> [Card] -> [Card]
        fEarlyHighLead handList memoryList
            | (length memoryList <= 12) = fSortByRankDesc (fClubs handList ++ fDiamonds handList)
            | otherwise = []
        -- Again similar to fSafeLead but this time we make sure the highest heart is not played prematurely
        fSafeLead2 :: [Card] -> [Card] -> [Card]
        fSafeLead2 handList memoryList
            | (heartsBroken memoryList) = fSortByRank $ fDiamonds handList ++ fClubs handList ++ fHeartsNoHighest handList ++ fSafeSpades handList
            | otherwise = fSortByRank $ fDiamonds handList ++ fClubs handList ++ fSafeSpades handList

-- ================================================================================================================= --
--                                                RENEGE FUNCTION                                                    --
--                   The Renege Function for a traditional 4v4 Hearts Battle Royale with 13 tricks                   --                                                              --
-- ================================================================================================================= --
{-How it works:
    Arrange the cards in a hand based on a pre-determined "priority list".
    The card to be played at any round will be the first card in this list.
    ----------[RENEGE PRIORITY LIST]----------
    [A] Non SQ holder
    ----------[HAS CARDS OF LEADER'S SUIT]----------
        1.) (First Round) Highest Club
            Reason: The first round is 100% safe from point cards therefore get rid of the highest club card
        2.) (Second & Third Round) Risky Club/Diamond
            Reason: The second & third rounds are relatively safe from point cards therefore get rid of high club/diamond cards
        3.) Highest "Safe" Card with Leader's Suit (Safe means lower than the highest same-suit card of the current trick)
            Reason: Staying out of lead for next round while conserving lower cards for later rounds
        4.) Lowest "Risky" Card with Leader's Suit
            Reason: If we can't play a card that is lower than the leader's card then play the next ranked card.
            Hope that the next players renegeng will be forced to play a higher card thus winning the trick.
    ----------[NO CARDS OF LEADER'S SUIT]----------
        5.) Queen of Spade
            Reason: Get rid of SQ as soon as possible.
        6.) Risky Spades (Spades > Queen)
            Reason: Risky Spades attract point cards during a Spade trick so it is wise to get rid of them.
        7.) Risky Diamonds & Clubs (Diamonds & Clubs > Jack)
            Reason: Risky Diamonds attract point cards during a Diamond trick.
        8.) Highest Hearts
            Reason: Break & Throw the highest hearts in non heart suit as it is safe.
        9.) Safe Diamonds & Clubs
            Reason: Safe non-point cards should be used to get out of leading not discarded in renege.
        10.) Safe Spades
            Reason: Safe Spades are useful to bleed spades and are a very useful card to get out of lead.

    [B] SQ holder
    ----------[HAS CARDS OF LEADER'S SUIT]----------
        1.) (First Round) Highest Club
            Reason: The first round is safe from point cards
        2.) Highest Diamond/Club/Spade(NOT SQ) Card matching the Leader's Suit
            Reason: Stay in lead in order to deplete either Diamond/Club to void either Suit ASAP
        3.) Lowest Heart Card matching the Leader's Suit
            Reason: It's better to not be in lead in a heart trick even if it means the next player would lead with spade/heart
    ----------[NO CARDS OF LEADER'S SUIT]----------
        4.) Queen of Spade
            Reason: Get rid of SQ as soon as possible.
        5.) Risky Spades (Spades > Queen)
            Reason: Risky Spades attract point cards during a Spade trick.
        6.) Risky Diamonds & Clubs (Diamonds & Clubs > Jack) 
            Reason: Risky Diamonds attract point cards during a Diamond trick.
        7.) Highest Hearts
            Reason: Break & Throw high hearts in non heart suit.
        8.) Safe Diamonds & Clubs
            Reason: Safe non-point cards should be used to get out of leading not discarded in renege.
        9.) Safe Spades
            Reason: Safe Spades are useful to survive Spade tricks and are a very useful card to lead.
-}
--          current trick    ->  hand  -> memory -> chosen card 
renege :: [(Card, PlayerId)] -> [Card] -> [Card] -> Card
renege trick hand memory
    -- SQ HOLDER
    | isHoldingSQ hand =
        -- SAME SUIT CARD IN HAND
        select $ (firstRound hand (leader trick) ++
        fSuitDumbSpade (suit $ leader trick) (simpleTrick trick) hand ++
        fSuitRenegeSQ (suit $ leader trick) hand ++
        fSuitSafe (suit $ leader trick) (simpleTrick trick) hand ++
        fSuitRisky (suit $ leader trick) (simpleTrick trick) hand ++
        -- NO SAME SUIT CARD IN HAND
        fSQRenege (leader trick) hand ++
        fRiskySpades hand ++
        fRiskyDC hand ++
        fHeartsRenege (leader trick) hand ++
        fSafeDCRenege hand ++
        (sortDesc $ fSpades hand))
    -- Non SQ Holder
    | otherwise =
        -- SAME SUIT CARD IN HAND
        select $ (firstRound hand (leader trick) ++
        secondThirdRound hand memory (suit $ leader trick) ++ 
        fSuitSafe (suit $ leader trick) (simpleTrick trick) hand ++
        fSuitRisky (suit $ leader trick) (simpleTrick trick) hand ++
        -- NO SAME SUIT CARD IN HAND
        fSQRenege (leader trick) hand ++
        fRiskySpades hand ++
        fRiskyDC hand ++
        fHeartsRenege (leader trick) hand ++
        fSafeDCRenege hand ++
        (sortDesc $ fSpades hand))
    where
    -- | Select the first card from hand if no particular card was provided.
    select :: [Card] -> Card
    select [] = head hand
    select x = head x
    ----------[SQ + Non SQ Holder Functions]----------
    -- If not holding Two of Clubs, filter for the highest club card
    firstRound :: [Card] -> Card -> [Card]
    firstRound handList leadCard
        | leadCard == (Card Club Two) = sortDesc $ fClubs handList
        | otherwise = []

    -- Filter for the highest cards which are lower in rank than the leader card
    fSuitSafe :: Suit -> [Card] -> [Card] -> [Card]
    fSuitSafe leadSuit thisTrick handList = sortDesc $ filter (\x -> suit x == leadSuit && rank x < (rank $ fHighestCard leadSuit thisTrick)) handList
    
    -- Filter for the lowest cards which are higher in rank than the leader card
    fSuitRisky :: Suit -> [Card] -> [Card] -> [Card]
    fSuitRisky leadSuit thisTrick handList = sort $ filter (\x -> suit x == leadSuit && rank x > (rank $ fHighestCard leadSuit thisTrick)) handList
    
    -- Modified filter for SQ which prevents it from being played in the first round (against game rules)
    fSQRenege :: Card -> [Card] -> [Card]
    fSQRenege (Card Club Two) _ = []
    fSQRenege _ handList = fSQ handList
    
    -- Modified filter for Hearts which prevents them from being played in the first round (against game rules) 
    fHeartsRenege :: Card -> [Card] -> [Card]
    fHeartsRenege (Card Club Two) _ = []
    fHeartsRenege _ handList = sortDesc $ fHearts handList

    -- Filters for the lowest Diamond/Club cards
    fSafeDCRenege :: [Card] -> [Card]
    fSafeDCRenege handList = fSortByRankDesc $ fDiamonds handList ++ fClubs handList

    ----------[Non SQ Holder Functions]----------
    -- Filters for the highest club/diamond cards but only if its the first 3 rounds
    secondThirdRound :: [Card] -> [Card] -> Suit -> [Card]
    secondThirdRound handList memoryList leadSuit
        | (length memoryList > 8) = []
        | (leadSuit == Club) = sortDesc $ fClubs handList
        | (leadSuit == Diamond) = sortDesc $ fDiamonds handList
        | otherwise = []
        
    ----------[SQ Holder Functions]----------
    -- Made especially to handle random/dumb ai that will play Risky spade > Queen in a Spade trick
    fSuitDumbSpade :: Suit -> [Card] -> [Card] -> [Card]
    fSuitDumbSpade suitTarget trickList handList
        | (suitTarget == Spade) && (fRiskySpades trickList /= []) = fSQ handList
        | otherwise = []

    -- Filters for lowest of heart cards matching suitTarget and highest of diamond, club and spades (excluding SQ) matching suit
    fSuitRenegeSQ :: Suit -> [Card] -> [Card]
    fSuitRenegeSQ suitTarget handList
        | fSQ handList == [] = []
        | suitTarget == Heart = fSortByRank $ fSuit suitTarget (fHearts handList) 
        | otherwise = fSortByRankDesc $ fSuit suitTarget (fDiamonds handList ++ fClubs handList ++ fSpadesNotSQ handList)
-- ================================================================================================================= --
--                                                RENEGE2 FUNCTION                                                   --
--                             The Renege Function for 1v1 Hearts game with 26 tricks                                --                                                              --
-- ================================================================================================================= --
{-How it works:
    Arrange the cards in a hand based on a pre-determined "priority list".
    The card to be played at any round will be the first card in this list.
    ----------[RENEGE PRIORITY LIST]----------
    [A] Early Aggressive + Mid-Late game Conservative + Sacrifice Highest Heart
    ----------[HAS CARDS OF LEADER'S SUIT]----------
        1.) (Early game first 8 rounds) Highest Club, Diamond, Spade matching Leader's Suit
            Reason: Point cards are rarely thrown away in the first 8 rounds of a 1v1 match so its safe to get rid of high cards.
        2.) Highest Safe card matching leader's suit excluding the Highest heart
            Reason: Like a 4v4, we want to avoid leading by playing any safe same suit card available.
        3.) Highest card matching leader's suit excluding the Highest heart
            Reason: Unlike a 4v4, if we know we are going to win a trick then might as well throw away the largest card of the same suit.
            This is because there is no chance of another player playing a higher card than us.
        4.) Highest of Heart
            Reason: If opponent leads with Heart and we only have highest of heart, we are forced to give it up.
    ----------[NO CARDS OF LEADER'S SUIT]----------
        5.) Queen of Spade
            Reason: Get rid of SQ ASAP
        6.) Risky Spades (Spades > Queen)
            Reason: Risky spades attract Queen of spades
        7.) Second Highest Hearts
            Reason: Get rid of high hearts so we can win heart tricks in the late game
            Make sure to still conserve the highest heart to ensure the opponent cannot shoot the moon.
        7.) Highest Diamonds, Clubs, Spades
            Reason: High cards attract point cards
        8.) Lowest Hearts (including Highest)
            Reason: Break & Throw the lowest hearts in non heart suit
            Make sure the highest heart is used last.
-}
renege2 :: [(Card, PlayerId)] -> [Card] -> [Card] -> Card
renege2 trick hand memory = 
    select $ (fEarlyRiskyRenege hand memory (suit $ leader trick) ++
    fSuitSafe (suit $ leader trick) (simpleTrick trick) hand ++
    fSuitHighest (suit $ leader trick) hand ++
    fSuit (suit $ leader trick) hand ++
    fSQ hand ++
    fRiskySpades hand ++
    (sortDesc $ fHeartsNoHighest hand) ++
    fSortByRankDesc (fClubs hand ++ fDiamonds hand ++ fSpades hand) ++
    (sort $ fHearts hand) ++
    sort hand
    )
    where
        select :: [Card] -> Card
        select [] = head hand
        select x = head x

        -- Filters for the highest diamond, clubs and safe spades (<Queen) if within the first 8 rounds
        fEarlyRiskyRenege :: [Card] -> [Card] -> Suit -> [Card]
        fEarlyRiskyRenege handList memoryList leadSuit
            | (length memoryList > 12) || (leadSuit == Heart) = []
            | otherwise = sortDesc $ fSuit leadSuit (fClubs handList ++ fDiamonds handList ++ fSafeSpades handList) 

        -- Filter for the highest same suit cards which are lower in rank than the leader card
        fSuitSafe :: Suit -> [Card] -> [Card] -> [Card]
        fSuitSafe leadSuit thisTrick handList = sortDesc $ filter (\x -> suit x == leadSuit && rank x < (rank $ fHighestCard leadSuit thisTrick)) $ fNotHighestHeart handList
        
        -- Filter for the highest same suit cards which are lower in rank than the leader card
        -- Exclude SQ and Highest heart
        fSuitHighest :: Suit -> [Card] -> [Card]
        fSuitHighest leadSuit handList = sortDesc $ fNotHighestHeart $ fNotSQ $ fSuit leadSuit handList
-- ================================================================================================================= --
--                                                HELPER FUNCTIONS                                                   --                                     --                                                              
-- ================================================================================================================= --
        
-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- | Given a card, select its rank.
rank :: Card -> Rank
rank (Card _ r) = r

-- | Given a list of cards and a suit, count cards belonging to the suit
count :: [Card] -> Suit -> Int
count hand suitTarget = length $ filter (\x -> suit x == suitTarget) hand

-- | Given a list of card, playerId tuples,select the lead card tuple
leader :: [(Card, PlayerId)] -> Card
leader trick = fst $ last trick

-- | Convenience function to sort card list in descending order
sortDesc :: [Card] -> [Card]
sortDesc hand = reverse . sort $ hand

-- | Simplifies the card tuple into a card list by removing PlayerId
simpleTrick :: [(Card, PlayerId)] -> [Card]
simpleTrick = map (\(a,_) -> a)

-- | Returns True if hearts is broken, False otherwise
heartsBroken :: [Card] -> Bool
heartsBroken memory = (fHearts memory) /= []

-- | Returns True if SQ holder, False otherwise
isHoldingSQ :: [Card] -> Bool
isHoldingSQ hand = (fSQ hand) /= []

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
