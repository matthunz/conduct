import Data.Maybe
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
  ( Args,
    UserHooks (confHook, preConf),
    defaultMainWithHooks,
    simpleUserHooks,
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (localPkgDescr),
  )
import Distribution.Simple.Setup
  ( BuildFlags (buildVerbosity),
    ConfigFlags (configVerbosity),
    fromFlag,
  )
import Distribution.Simple.UserHooks
  ( UserHooks (buildHook, confHook),
  )
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Verbosity (normal)
import System.Directory (getCurrentDirectory)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = rustConfHook
      }

rustConfHook ::
  (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
  ConfigFlags ->
  IO LocalBuildInfo
rustConfHook (description, buildInfo) flags = do
  -- rawSystemExit normal "cargo" ["build", "--release"]
  rawSystemExit normal "cargo" ["build"]

  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { PD.library =
                Just $
                  library
                    { PD.libBuildInfo =
                        libraryBuildInfo
                          { PD.extraLibDirs =
                              (dir ++ "/target/release")
                                : (dir ++ "/target/aarch64-apple-darwin/release")
                                : (dir ++ "/target/debug")
                                : (dir ++ "/target/aarch64-apple-darwin/debug")
                                : PD.extraLibDirs libraryBuildInfo
                          }
                    }
            }
      }
