/**
 * cse250.pa2.CircularQueueTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * Tanvirul Islam
 */

package testing

import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._
import queue.CircularQueue

class CircularQueueTests extends FlatSpec with BeforeAndAfter {
  behavior of "CircularQueue"

  it should "follow the example provided in the handout" in {
    val queueVal = new CircularQueue[Int](4); //capacity: 4

    queueVal.enqueue(0);
    queueVal.enqueue(1);
    queueVal.enqueue(2);
    queueVal.enqueue(3);
    queueVal.dequeue(); // returns 0
    queueVal.dequeue(); // returns 0
    queueVal.enqueue(4);
    queueVal.enqueue(5);
    queueVal.front; // returns 2
    for(e <- queueVal) println(e); // prints 2 3 4 5
    queueVal.enqueue(6); // triggers reserve.
  }

  /*
  behavior of "Queue 0 capacity"

  it should "idk" in {
    val queue = new CircularQueue[Int](0)
    assert(queue._capacity == 0)
    queue.enqueue(0)
    assert(queue._capacity == 0)
  }
  */

  behavior of "Empty queue"

  it should "have the correct invariants" in {
    val queue = new CircularQueue[Int](5)
    assert(queue._capacity == 5)
    assert(queue._lowerBound == -1)
    assert(queue._upperBound == -1)
  }

  it should "return the correction exception for apply" in {
    val queue = new CircularQueue[Int](4)
    assertThrows[IllegalArgumentException](queue.apply(0))
    assertThrows[IllegalArgumentException](queue.apply(1))
  }

  it should "return the correct exception for front" in {
    val queue = new CircularQueue[Int](4)
    assertThrows[IllegalArgumentException](queue.front)
    assertThrows[IllegalArgumentException](queue.front)
  }

  it should "return the correction exception for dequeue" in {
    val queue = new CircularQueue[Int](4)
    assertThrows[IllegalArgumentException](queue.dequeue())
    assertThrows[IllegalArgumentException](queue.dequeue())
  }

  it should "properly use the iterator" in {
    val queue = new CircularQueue[Int](4)
    val iter = queue.iterator
    assert(!iter.hasNext)
  }

  behavior of "apply"
  it should "return the correct index when there is no dequeue" in {
    val queue = new CircularQueue[Int](1)
    var counter = 0
    for(i <- 0 to 31) {
      queue.enqueue(i)
      assert(queue.apply(counter) == counter)
      counter += 1
    }
  }

  it should "return the correct index with dequeue into empty queue" in {
    val queue = new CircularQueue[Int](1)
    var counter = 0
    for(i <- 0 to 31) {
      queue.enqueue(i)
      assert(queue.apply(0) == counter)
      //assert(queue._lowerBound == 0)
      //assert(queue._upperBound == 1)
      queue.dequeue()
      assert(queue._lowerBound == -1)
      assert(queue._upperBound == -1)
      counter += 1
    }
  }

  it should "return the correct index after enqueue" in {
    val queue = new CircularQueue[Int](1)
    var counter = 0
    for(i <- 0 to 31) {
      queue.enqueue(i)
      assert(queue.apply(i) == counter)
      //assert(queue._lowerBound == 0)
      //assert(queue._upperBound == counter + 1)
      counter += 1
    }
  }

  behavior of "length"
  it should "return the correct length" in {
    val queue = new CircularQueue[Int](10)
    assert(queue.length == 0)
    for(i <- 0 to 31) {
      queue.enqueue(i)
      assert(queue.length == i + 1)
    }
  }

  behavior of "iterator"
  it should "return the proper values on next()" in {
    val queue = new CircularQueue[Int](1)
    var counter = 0;

    for(i <- 0 to 63) queue.enqueue(i)

    val iter = queue.iterator
    while(iter.hasNext) {
      val returnValue = iter.next()
      assert(returnValue == counter)
      counter += 1
    }

    for(i <- 0 to 63) queue.dequeue()
    val iter2 = queue.iterator
    assert(!iter2.hasNext)
  }

  it should "return false when queue is emptied" in {
    val queue = new CircularQueue[Int](1)

    for(i <- 0 to 63) queue.enqueue(i)
    for(i <- 0 to 63) queue.dequeue()

    val iter = queue.iterator
    assert(!iter.hasNext)
  }

  behavior of "enqueue"
  it should "insert the proper values in the proper positions" in {
    val queue = new CircularQueue[Int](1)
    for(i <- 0 to 31) {
      queue.enqueue(i)
      assert(queue._upperBound != -1)
      assert(queue._lowerBound != -1)
      assert(queue.apply(i) == i)
      //assert(queue._upperBound == i + 1)
      assert(queue.length == i + 1)
    }
    assertThrows[IllegalArgumentException](queue.apply(100))

  }

  behavior of "dequeue"
  it should "return the correct value after dequeue" in {
    val queue = new CircularQueue[Int](1)
    for(i <- 0 to 31) queue.enqueue(i)
    for(i <- 0 to 31) {
      assert(queue._upperBound != -1)
      assert(queue._lowerBound != -1)
      assert(queue.apply(0) == i)
      //assert(queue._lowerBound == i)
      assert(queue.dequeue() == i)
      //if(i != 31) assert(queue._lowerBound == i + 1)
    }
    assert(queue._lowerBound == -1)
    assert(queue._upperBound == -1)
    assertThrows[IllegalArgumentException](queue.dequeue())
  }

  behavior of "reserve"
  it should "properly reserve" in {
    val queue = new CircularQueue[Int](1)
    queue.enqueue(0)
    assert(queue._capacity == 1)
    queue.enqueue(1)
    assert(queue._capacity == 2)
    queue.enqueue(2)
    assert(queue._capacity == 4)
    queue.enqueue(3)
    queue.enqueue(4)
    assert(queue._capacity == 8)
    for(i <- 5 to 15) queue.enqueue(i)
    assert(queue._capacity == 16)
    queue.enqueue(16)
    assert(queue._capacity == 32)
    for(i <- 17 to 31) queue.enqueue(i)

    for(i <- 0 to 31) assert(queue.apply(i) == i)
  }

  behavior of "front"
  it should "return the proper front value" in  {
    val queue = new CircularQueue[Int](1)
    for(i <- 0 to 31) {
      queue.enqueue(i)
      assert(queue.front == 0)
    }

    for(i <- 0 to 31) {
      assert(queue.front == i)
      assert(queue.dequeue() == i)
    }
    assertThrows[IllegalArgumentException](queue.front)
  }

  behavior of "everything"
  it should "properly adjust the queue" in {
    val queue = new CircularQueue[Int](1)
    assert(queue._capacity == 1)

    queue.enqueue(0)
    assert(queue.head == 0)
    assert(queue._capacity == 1)
    assert(queue.length == 1)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.head)

    queue.enqueue(1)
    assert(queue.apply(1) == 1)
    assert(queue._capacity == 2)
    assert(queue.length == 2)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(1))

    queue.enqueue(2)
    assert(queue.apply(2) == 2)
    assert(queue._capacity == 4)
    assert(queue.length == 3)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(2))

    queue.enqueue(3)
    assert(queue.apply(3) == 3)
    assert(queue._capacity == 4)
    assert(queue.length == 4)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(3))

    assert(queue.dequeue() == 0)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(2))
    assert(queue._capacity == 4)
    assert(queue.length == 3)

    assert(queue.dequeue() == 1)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(1))
    assert(queue._capacity == 4)
    assert(queue.length == 2)

    queue.enqueue(4)
    println(queue._dataArray(queue._lowerBound))
    println(queue._dataArray(queue._upperBound - 1))
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(2))
    assert(queue._capacity == 4)
    assert(queue.length == 3)

    queue.enqueue(5)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(3))
    assert(queue._capacity == 4)
    assert(queue.length == 4)

    var counter = 2;
    for(i <- queue) {
      assert(i == counter)
      counter += 1
    }

    queue.enqueue(6)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(4))
    assert(queue._capacity == 8)
    assert(queue.length == 5)

    counter = 2;
    for(i <- queue) {
      assert(i == counter)
      counter += 1
    }

    queue.enqueue(7)
    assert(queue._capacity == 8)
    assert(queue._dataArray(queue._lowerBound) == queue.head)
    assert(queue._dataArray(queue._upperBound - 1) == queue.apply(5))
    assert(queue.length == 6)

    counter = 2;
    for(i <- queue) {
      assert(i == counter)
      counter += 1
    }
  }

  behavior of "String"
  it should "properly check strings" in {
    val queue = new CircularQueue[String](1)
    for(i <- 0 to 31) {
      queue.enqueue(i.toString)
      assert(queue.head == i.toString)
      assert(queue.length == 1)
      assert(queue.dequeue() == i.toString)
      assert(queue.length == 0)
    }

    for(i <- 0 to 31) {
      queue.enqueue(i.toString)
      assert(queue.length == i + 1)
      assert(queue.apply(i) == i.toString)
      assert(queue.front == queue.head)
    }

    var counter = 0;
    for(i <- queue) {
      assert(i == counter.toString)
      counter += 1
    }
  }
}
