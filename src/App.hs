module App where

import Args ( Args (..), parseArgs, Command (..), Model (..), Exercise (..) )
import Options.Applicative ( handleParseResult )
import System.Environment (getArgs)
import qualified OpenAiO3High as OpenAiO3High
import qualified DeepSeekR1 as DeepSeekR1

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO()
program' (Args _ Doctor) = print "doctor"
program' (Args _ (Run BouncingBalls O3High)) = OpenAiO3High.main
program' (Args _ (Run BouncingBalls DSR1)) = DeepSeekR1.main
