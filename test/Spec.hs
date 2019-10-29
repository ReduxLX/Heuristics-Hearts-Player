import Control.Monad
import System.Exit

import EitherIO
import Hearts.Types
import Hearts.Play(playGame)
import Game(newPlayer)
import Logs(writeGame)
import Deck
import Cards

import safe qualified Player


-- | Test a game of the Player against itself.
test_one_play :: [Player] -> IO Bool
test_one_play players = do
  played <- runEitherIO $ playGame 100 players
  case played of
    Right gr -> writeGame gr >> return True
    -- Beware, you need to capture the error if run headless
    Left e -> putStrLn "" >> print e >> return False

main :: IO ()
main = do
  played <- test_one_play $ player <$> ["Felix Jellyburger", "Link Nealydoo", "Rhett Mcloughfyloff", "Toadalbrisket"]
  if played
     then exitSuccess
     else exitFailure
  where
    player = \x -> newPlayer x Player.playCard Player.makeBid
