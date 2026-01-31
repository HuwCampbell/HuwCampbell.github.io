---
title: Optimised Row Columnar for Haskell
description: A new library for reading and writing Optimised Row Columnar files in native Haskell
tags: Haskell
date: 2020-09-18
atUri: "at://did:plc:6e7msnu5o3mdboitzv7oxbxm/site.standard.document/3mdos56pm552j"
---

I are proud to announce a new [Apache ORC](https://github.com/HuwCampbell/orc-haskell) library for Haskell which I am releasing as open source under the Affero General Public License. This project was written as a component for the [Icicle](https://icicle-lang.github.io) query language; and this announcement is cross posted there.

Orc-Haskell supports all column types and encodings as well as snappy, zlib, and zstd compression formats.

We've used hedgehog's round-trip testing to ensure consistency between reading and writing, golden tests against the specification, and integration tests with the C++ implementation using the examples from the ORC repository.


#### Optimised Row Columnar

Apache ORC is an extremely useful file format, it's Hadoop native, usable with Hive, Spark, Presto, and Athena; and offers great compression and locality.  Unfortunately though, it is complex, and until now reading and writing ORC files in Haskell has not been practical.

This library is implemented completely independently to the C++ and Java versions, and is based almost entirely on the [specification](https://orc.apache.org/specification/ORCv1/). This approach has allowed us to be a lot more succinct and idiomatic than a direct port would have been, and uncovered a small raft of mistakes and omissions from the specification which we will be reporting to the ORC maintainers.



#### API

As Icicle itself uses a column representation in its calling convention to C, we had to make a big deviation to how we parse data compared to the C++ and Java versions. The main difference is that our parser returns a columnar representation of each stripe directly, and thus reads the whole stripe into memory. The mainline versions return a record based reader, and seek through the file more in order to keep memory usage lower. Our approach has enabled use to use a layered API and allows for a much more streamlined code base, but does mean that RAM use is a higher than it would be had we used a different approach.

Most users won't want to deal with stripes directly though, and should import `Orc.Logical` with the functions

```haskell
-- | Open an ORC file and stream its values values as logical rows.
withOrcFile
  :: FilePath
  -- ^ The ORC file to open
  -> (Type -> (Stream (Of Row) IO ()) -> IO r)
  -- ^ How to consume the stream of values as a continuation
  -> IO r

-- | Write a stream of values as an ORC file.
putOrcFile
  :: Type
  -- ^ The type of the rows
  -> Maybe CompressionKind
  -- ^ An optional compression standard to use
  -> Int
  -- ^ The number of rows in each stripe
  -> FilePath
  -- ^ The filepath to write to
  -> Stream (Of Row) IO ()
  -- ^ The stream of rows to write
  -> IO ()
```

Where `Stream` is from the [streaming](http://hackage.haskell.org/package/streaming) library and `Row` is an algebraic data type representing a single row of an ORC file.

The layering mentioned is that these functions are themselves implemented using the `Orc.Striped.withOrcFileLifted` and `Orc.Striped.putOrcFileLifted` functions, which allow direct access to the striped representation.

#### Benchmarks

We use microbenchmarking for key functions, and have used optimised C code where performance is critical. The library is a lot newer than the C++ and Java versions though, and is in general a bit slower at parsing files.

Reading a 200 megabyte compressed file with 4 million rows and 20 columns takes roughly double to time to parse into a striped representation as the C++ version takes to read it at; pivoting to a logical stream brings it to a factor of 3 times slower.

Writing is very fast, adding only 12s when roundtripping the file through a striped representation.

<center>
|                  | C++      | Striped | Logical  |
|------------------|----------|---------|----------|
| Read only        | 18s      | 35s     | 50s      |
| Roundtrip        |          | 47s     | 1m 52s   |
</center>


#### Have Fun

Hopefully you find this library useful. ORC is a great format to use during ETL and for long term storage, as its type system is powerful enough to encode a lot of invariants, and its compatibility with tools like Spark and Presto make it quite flexible.

If you would like to include this library in a proprietary product, please reach out to me to discuss licensing beyond the AGPL-3.
