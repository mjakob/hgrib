import Control.Exception ( assert )
import Data.Grib
import Text.Printf       ( printf )


main :: IO ()
main = let filename = "test/stage/regular_latlon_surface.grib1" in
  runGribIO_ filename $ do
    setString "file" filename

    getLong "Ni" >>= liftIO . printf "numberOfPointsAlongAParallel=%d\n"
    getLong "Nj" >>= liftIO . printf "numberOfPointsAlongAMeridian=%d\n"

    getDouble "yFirst" >>=
      liftIO . printf "latitudeOfFirstGridPointInDegrees=%g\n"
    getDouble "xFirst" >>=
      liftIO . printf "longitudeOfFirstGridPointInDegrees=%g\n"
    getDouble "yLast"  >>=
      liftIO . printf "latitudeOfLastGridPointInDegrees=%g\n"
    getDouble "xLast"  >>=
      liftIO . printf "longitudeOfLastGridPointInDegrees=%g\n"
    getDouble "DyInDegrees" >>=
      liftIO . printf "jDirectionIncrementInDegrees=%g\n"
    getDouble "DxInDegrees" >>=
      liftIO . printf "iDirectionIncrementInDegrees=%g\n"

    getString "packingType" >>= liftIO . printf "packingType=%s\n"

    values <- getValues
    let numValues = length values
        average   = sum values / fromIntegral numValues
    liftIO $ printf "There are %d values, average is %g\n" numValues average

    filename' <- getString "file"
    liftIO $ assert (filename' == filename) (return ())
