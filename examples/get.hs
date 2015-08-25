import Control.Exception (assert)
import Data.Grib.Raw
import Foreign           (allocaArray, allocaBytes)
import Text.Printf       (printf)


filename :: FilePath
filename = "test/stage/regular_latlon_surface.grib1"

assertIO :: Bool -> IO ()
assertIO = flip assert $ return ()

main :: IO ()
main = do
  h <- withBinaryCFile filename ReadMode $
         gribHandleNewFromFile defaultGribContext

  _ <- gribSetString h "file" filename

  gribGetLong h "Ni" >>= printf "numberOfPointsAlongAParallel=%d\n"
  gribGetLong h "Nj" >>= printf "numberOfPointsAlongAMeridian=%d\n"

  gribGetDouble h "yFirst" >>= printf "latitudeOfFirstGridPointInDegrees=%g\n"
  gribGetDouble h "xFirst" >>= printf "longitudeOfFirstGridPointInDegrees=%g\n"
  gribGetDouble h "yLast"  >>= printf "latitudeOfLastGridPointInDegrees=%g\n"
  gribGetDouble h "xLast"  >>= printf "longitudeOfLastGridPointInDegrees=%g\n"
  gribGetDouble h "DyInDegrees" >>= printf "jDirectionIncrementInDegrees=%g\n"
  gribGetDouble h "DxInDegrees" >>= printf "iDirectionIncrementInDegrees=%g\n"

  len <- gribGetLength h "packingType"
  allocaBytes len $ \bufr -> do
    packingType <- gribGetString h "packingType" bufr len
    printf "packingType=%s (%d)\n" packingType (length packingType + 1)

  size <- gribGetSize h "values"
  allocaArray size $ \array -> do
    values <- gribGetDoubleArray h "values" array size
    let average = sum values / (fromIntegral . length $ values)
    printf "There are %d values, average is %g\n" size average

  len' <- gribGetLength h "file"
  assertIO $ len' == 1 + length filename
  allocaBytes len' $ \bufr' -> do
    file <- gribGetString h "file" bufr' len'
    assertIO $ file == filename
