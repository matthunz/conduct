import Distribution.Simple
import System.Process

main = do
  _ <- system "cargo build"
  defaultMain
