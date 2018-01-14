import           Control.Monad (unless)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import           System.Exit (exitFailure)

import qualified Test.Hedgehog.Text
import qualified Test.Hedgehog.Timeout


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.Hedgehog.Text.tests
    , Test.Hedgehog.Timeout.tests
    ]

  unless (and results) $
    exitFailure
