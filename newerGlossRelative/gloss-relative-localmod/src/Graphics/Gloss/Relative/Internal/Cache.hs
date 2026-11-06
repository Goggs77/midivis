module Graphics.Gloss.Relative.Internal.Cache where

import Graphics.Gloss.Relative.Internal.Window
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Data
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import System.Mem.StableName
import Control.Monad.Trans

-- | Cache entries are keyed by the screen size, the region name and the /hash/ of the frame's
-- 'StableName'. Hashes can collide, so the 'StableName' itself is kept in the value and checked
-- with 'eqStableName' on lookup; a collision is then simply treated as a cache miss.
type CacheKey = (ScreenSize,String,Int)
type CacheVal = (WindowOutput,Int,StableName Frame)
type CacheTable = IORef (Map CacheKey CacheVal)

newCacheTable :: IO CacheTable
newCacheTable = newIORef Map.empty

insertCacheTable :: Int -> CacheTable -> CacheKey -> StableName Frame -> WindowOutput -> IO ()
insertCacheTable i cache key sn val =
    atomicModifyIORef' cache $ \m -> (Map.insert key (val,i,sn) m, ())

-- | Looks up an entry, refreshing its last-used frame index. The whole read-modify-write is done
-- atomically, so concurrent insertions are not lost.
lookupCacheTable :: Int -> CacheTable -> CacheKey -> StableName Frame -> IO (Maybe WindowOutput)
lookupCacheTable i cache key sn = atomicModifyIORef' cache $ \m ->
    case Map.lookup key m of
        Just (v,_,sn') | eqStableName sn sn' -> (Map.insert key (v,i,sn') m, Just v)
        _ -> (m, Nothing)

cachedRenderFrame :: Int -> CacheTable -> (String -> Frame -> Window IO ()) -> String -> Frame -> Window IO ()
cachedRenderFrame _ _ render label f = do
    -- The frame is rebuilt every tick, so a StableName-based cache key can
    -- never hit: every lookup would be a miss and every frame would insert a
    -- new entry, accumulating up to simResolution dead WindowOutputs in the
    -- old generation.  That growth made major GC sweeps pause the audio
    -- callback thread (up to 70ms under load).  Render directly instead:
    -- the output is short-lived, collected in the nursery, no heap growth.
    dim <- askDimension
    wo <- lift $ execWindow dim (render label f)
    tellWindowOutput wo

evictOldCacheTable :: Int -> Int -> CacheTable -> IO ()
evictOldCacheTable frame maxAge cache =
    atomicModifyIORef' cache $ \m -> (Map.filter (\(_,used,_) -> frame - used <= maxAge) m,())
