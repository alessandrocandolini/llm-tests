module App where

import Args ( Args (..), parseArgs, Command (..), Model (..), Exercise (..) )
import Options.Applicative ( handleParseResult )
import System.Environment (getArgs)
import qualified OpenAiO3High as BBO3High

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO()
program' (Args _ Doctor) = print "doctor"
program' (Args _ (Run BouncingBalls O3High)) = BBO3High.main
