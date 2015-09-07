# HGrib Change Log

## 0.2.0.0

* Extended the compatible version range of GRIB API and the Haskell
  base library.

* Added a `gribGetApiVersion` function to `Data.Grib.Raw`.

* Included the `Data.Grib.Raw.Exception` module in `Data.Grib.Raw` and
  removed the marshal function `withGribMultiHandle`.

* Removed

  * `gribContextSetMemoryProc`
  * `gribContextSetPersistentMemoryProc`
  * `gribContextSetBufferMemoryProc`
  * `gribContextSetPrintingProc`
  * `gribContextSetLoggingProc`

  and corresponding foreign function type definitions from
  `Data.Grib.Raw` to be able to mark all remaining functions _unsafe_
  from a FFI perspective.

* Marked all foreign imports _unsafe_.

* Integrated the source code repository with [Travis CI][].

* Abandoned the plans to include the full GRIB API in `grib_api.h`
  (but requests to include specific parts are welcome).


## 0.1.0.0

* Initial release.


[Travis CI]: https://travis-ci.org/mjakob/hgrib
