# HGrib

Unofficial bindings for [ECMWF][]'s [GRIB API][] library for reading
[WMO FM-92 GRIB][] edition 1 and edition 2 messages.

In this version of HGrib, only raw bindings for the
[documented][GRIB Docs] part of GRIB API is available.  The next
versions are intended to include, in chronological order,

  * Raw bindings for the full API in the [grib_api.h][GRIB Header] C
    header file.

  * A higher-level Haskell interface.


## Installation

The following prerequisites are needed to build HGrib:

  * [GRIB API][] == 1.14.0 installed and discoverable by ghc (use
    cabal's `--extra-include-dirs` and `--extra-lib-dirs` if it's
    installed at a custom location); and

  * Haskell [base][] == 4.8.*

  * [c2hs][] == 0.26.*

With these requirements available, HGrib can be installed from
[Hackage][] with [Cabal][]:

```
cabal install hgrib
```


## Usage

Right now, only raw bindings for the [documented][GRIB Docs] part of
GRIB API is available in `Data.Grib.Raw`.  Much of the documentation
is copied into HGrib's [reference documentation][HGrib Docs] generated
by [Haddock][].  To be able to work with these bindings, bindings for
C's `fopen` is provided in `Data.Grib.Raw.CFile` (which is re-exported
by `Data.Grib.Raw`).  An example of usage is GRIB API's
[get.c][GRIB Get] example re-written with HGrib's bindings:

```haskell
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
```


## Contributing

Issues and pull requests are most welcome!


## Licenses

HGrib was created and is currently maintained by
[Mattias Jakobsson][].  It is released under the
[GNU General Public License v3.0][GPL3].  ECMWF's GRIB API is released
under the [Apache license][].  **HGrib is in no way associated with
ECMWF or the original library.**


[Apache license]:    https://software.ecmwf.int/wiki/display/GRIB/License
[base]:              http://hackage.haskell.org/package/base
[c2hs]:              https://github.com/haskell/c2hs
[Cabal]:             https://www.haskell.org/cabal/
[ECMWF]:             http://www.ecmwf.int/
[GPL3]:              http://www.gnu.org/licenses/gpl-3.0.html
[GRIB API]:          https://software.ecmwf.int/wiki/display/GRIB/Home
[GRIB Docs]:         https://software.ecmwf.int/wiki/display/GRIB/Module+Index
[GRIB Get]:          https://software.ecmwf.int/wiki/display/GRIB/get.c
[GRIB Header]:       https://software.ecmwf.int/wiki/display/GRIB/grib_api.h+File+Reference
[Hackage]:           http://hackage.haskell.org/
[Haddock]:           https://www.haskell.org/haddock/
[HGRIB Docs]:        https://hackage.haskell.org/package/hgrib
[Mattias Jakobsson]: https://github.com/mjakob
[WMO FM-92 GRIB]:    http://www.wmo.int/pages/prog/www/WMOCodes/Guides/GRIB/Introduction_GRIB1-GRIB2.pdf
