import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)
import System.Directory (createDirectoryIfMissing)
import System.Process

main = defaultMainWithHooks myHooks
  where myHooks = simpleUserHooks { postBuild = myPostBuild
                                  }

myPostBuild  _ _ _ _ = do
  _ <- spawnCommand "hasktags -c ."
  return ()
