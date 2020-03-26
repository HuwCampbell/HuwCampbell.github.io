---
title: Dumb Spark Tricks - Partitioning
description: How to force Spark to not repartition in Streaming and Non-Streaming applications
tags: spark
---

I've been working with Spark for a few years now, and have pushed it into realms where it was not
really meant to go. Over this time, I have written a lot of custom aggregators, catalyst expressions,
custom RDDs and streaming interfaces.

This series is to share some of the interesting, painful, and downright silly things which I have had
been forced to do.

We're going to kick off with a relatively simple one.

#### How to trick Spark into not doing any repartitioning when we know the data layout

I needed to join two datasets, but I knew my input data (from Kafka) was partitioned in a way that meant
I could union and join without a shuffle step. In my use case the data volumes were legitimately large,
and were coming in streaming. Any delay based on shuffling data between nodes would mean we would fall
behind and probably never catch up.

Fortunately, after my colleague and I requested it, our input data was partitioned using the same join
key; unfortunately, Spark couldn't know that in advance.

For various reasons, we were using RDDs instead of Dataframes, and the solution I came up with was to
wrap the Kafka RDDs inside new ones, which enforced a custom "never ever shuffle" partitioner.

The API for writing RDDs looks something like this:

```scala
abstract class RDD[T: ClassTag](
    private var _sc: SparkContext,
    private var deps: Seq[Dependency[_]]
  ) {
    // Just a helper function for building single child RDDs
    def this(oneParted: RDD[_]) =
      this(oneParent.context, List(new OneToOneDependency(oneParent)))

    def compute(split: Partition, context: TaskContext): Iterator[T]

    protected def getPartitions: Array[Partitions]

    val partitioner: Option[Partitioner] = None
  }
```

What we want to do it create a new RDD, which acts just like the old one,
but enforcing a custom partitioner; and this is actually really pretty simple:

```scala
case class UsingPartitioner[T: ClassTag](parent: RDD[T], _partitioner: Partitioner)
  extends RDD[T](parent) {

  override def compute(split: Partition, context: TaskContext): Iterator[T] =
    parent.compute(split, context)

  override protected def getPartitions: Array[Partitions] =
    parent.partitions

  override val partitioner: Option[Partitioner] =
    Some(_partitioner)
}
```

Now, to make sure that Spark is never going to perform a shuffle, we can write a little
partitioner which is just a dummy, but makes sure than any attempts to shuffle will
throw an exception.

```scala
case class FreezeNode(numNode: Int) extends Partitioner {
  def numPartitions: Int =
    numNode

  def getPartition(key: Any): Int =
    throw new Exception("Spark is trying to perform a shuffle!")
}
```

Now we are able to, if we know our partitioning strategy in advance, write joins which
can not repartition the data and guarantee efficient joins.


```scala
val frozenLeft: RDD[(K, W)] =
  UsingPartitioner(FreezeNode(5)), left)

val frozenRight RDD[(K, X)] =
  UsingPartitioner(FreezeNode(5)), right)

val joined: RDD[(K, (W, X))]=
  frozenLeft join frozenRight
```

We know that spark can not have performed a shuffle when we request data from
the joined dataset. Obviously, correctness is now up to us, as if our partitioning
is wrong, we'll certainly and silently drop data.



#### Join Free Streaming Applications

For Streaming applications Spark uses DStreams as the base class. These are *relatively*
simple wrappers over RDDs, and one can use `transform` to wrap the internally computed
RDDs with the frozen partitioner.


#### Conclusion

Spark provides optimisations which can make safe joins more efficient, but we can hijack
them if we really need to to make our unsafe joins also fast.

Join me next time, when we'll talk about Streaming applications, and how to deal with
funky time discrepancies.

