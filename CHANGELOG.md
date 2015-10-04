# HGrib Change Log

## 0.3.0.0

* Added a `GribIO` monad in `Data.Grib`, which is a higher-level
  abstraction for reading GRIB files over the raw bindings in
  `Data.Grib.Raw`.

* Moved `Data.Grib.Raw.Exception` up to `Data.Grib`.

* `Data.Grib.Raw` no longer re-exports the `Exception` module
  mentioned above.

* Made `gribHandleNewFromFile` return a `Maybe GribHandle` instead of
  a plain `GribHandle`.  `Nothing` is returned if no more messages
  could be read from the given stream.

* Re-organized the test utility modules.

* Moved `Data.Grib.Raw.Marshal` from other to exposed modules, but it
  should still be considered an internal module and it is not included
  in the documentation.


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
