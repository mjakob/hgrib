# HGrib

[![Build Status](https://travis-ci.org/mjakob/hgrib.svg?branch=develop)](https://travis-ci.org/mjakob/hgrib)

Unofficial bindings for [ECMWF][]'s [GRIB API][] library for reading
[WMO FM-92 GRIB][] edition 1 and edition 2 messages.

In this version of HGrib, raw bindings for the [documented][GRIB Docs]
part of GRIB API is available.  Future versions are intended to
include a higher-level Haskell interface.


## Installation

The following prerequisites are needed to build HGrib:

  * [GRIB API][] >= 1.12 installed and discoverable by ghc (use
    cabal's `--extra-include-dirs` and `--extra-lib-dirs` if it's
    installed at a custom location); and

  * Haskell [base][] >= 4.5; and

  * [c2hs][] == 0.26.*.

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
```


## Contributing

Issues and pull requests are most welcome!  In particular, let me know
if there is any undocumented part of GRIB API that you would like to
have included.


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
[Hackage]:           http://hackage.haskell.org/
[Haddock]:           https://www.haskell.org/haddock/
[HGRIB Docs]:        https://hackage.haskell.org/package/hgrib
[Mattias Jakobsson]: https://github.com/mjakob
[WMO FM-92 GRIB]:    http://www.wmo.int/pages/prog/www/WMOCodes/Guides/GRIB/Introduction_GRIB1-GRIB2.pdf
