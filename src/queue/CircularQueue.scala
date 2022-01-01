/**
 * queue.CircularQueue.scala
 *
 * Copyright 2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * Tanvirul Islam
 */
package queue

import types.mutable.QueueADT
import scala.reflect.ClassTag

class CircularQueue[A: ClassTag] (var _capacity: Int) extends QueueADT[A] with collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "CircularQueue"

  // Member variables
  var _dataArray: Array[A] = new Array(_capacity)
  var _numStored: Int = 0
  var _lowerBound: Int = -1
  var _upperBound: Int = -1
  var _headIndex: Int = 0

  /**
   * Gets element at position idx within the sequence.
   * @param idx index to check and return for value
   **/
  override def apply(idx: Int): A = {
    /**
     * Check if the index is within the valid range, if it isn't, throw an exception.
     *
     * Next, check if lower bound + idx is less than the capacity because if it isn't, return the value at that array position
     * would return an index out of bounds exception. The reason why it's necessary to add them together to get the correct value is
     * because the idx value is the value idx positions away from the lower bound, the start of the queue.
     * If the lower bound and idx together are larger than the capacity, instead you take the difference of capacity and lower bound, and
     * go to the element at the idx value minus that difference, it's necessary here to get the difference because it represents
     * the amount of spaces already accounted for when moving to the specified index.
     */
    if(idx >= _numStored || idx < 0) {
      throw new IllegalArgumentException("Provided an invalid index")
    }

    if(_lowerBound + idx < _capacity) {
      _dataArray(_lowerBound + idx)
    } else {
      val difference = _capacity - _lowerBound //The difference that must be taken away from idx to get the final number
      _dataArray(idx - difference)
    }

  }

  /** Gets the number of elements stored within the sequence. */
  override def length: Int = _numStored

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = new Iterator[A] {

    /**
     * When a new iterator is created, it sets the current position of that array to the lower bound, the start of the queue and sets an initial
     * value, this value is used to check if the iterator is on its first iteration.
     *
     * When hasNext is called it first checks that currentPos isn't equal to -1, if it is, then the array is empty,
     * so false is returned, otherwise it checks if this is the very first iteration, because if it is currentPos will be equal to _upperbound if the queue is full,
     * making hasNext false rather than true, this stops that from happening, and should always return true, because the only scenario that conditional would be hit
     * in is when the queue is full.
     *
     * When next is called, the returnValue is assigned to the value in _dataArray at the current position, and then currentPosition is incremented. If the
     * currentPos is at the very end of the array, and the lower bound is greater than or equal to the upper bound currentPos is set to 0
     * because the next value in the queue is at the front of the array. So, in this case that would mean that the upperbound has not yet been reached (the queue
     * has not been fully traversed) because the incrementing currentpos couldn't have reached the upper bound yet because it stared at a higher value.
     * If these conditions are false, currentPos is incremented as expected and returnValue is returned instead.
     *
     * This is the expected implementation of the iterator, as long as hasNext is true, you are able to run the next function without issue
     */
    var currentPos: Int = _lowerBound  //Position of the queue start
    var initial: Boolean = true //Check iterator has already started traversing the queue

    override def hasNext: Boolean = {
      if(currentPos == -1) {
        false
      } else if(initial && currentPos == _upperBound) {
        initial = false
        true
      } else {
        initial = false
        currentPos != _upperBound
      }
    }

    override def next(): A = {
      val returnValue: A = _dataArray(currentPos)
      if(currentPos == _capacity - 1 && _lowerBound >= _upperBound) currentPos = 0
      else currentPos += 1
      returnValue
    }
  }

  /** Reserves twice the capacity as the original Array. */
  private def reserve(): Unit = {

    /**
     * This assigns the current _dataArray array to the variable currentArray, it then doubles the capacity and creates
     * a new array using that changed capacity, and copies over all of the elements from the previous array into the
     * new array using the for loop. The for loop copies over values, but adjusts the positions so that the oldest values
     * are the furthest left with the youngest on the furthest right, this will help to maintain proper ordering for
     * enqueue, and dequeue.
     * After that, _dataArray is set to the new array
     */
    val currentArray: Array[A] = _dataArray //The current array that has reach max capacity
    _capacity *= 2
    val newArray: Array[A] = new Array[A](_capacity) //The new array with double the capacity

    var currentPos: Int = _lowerBound //This value will be used to keep the queue in order, it is the position in the currentArray
    var newArrayLocation: Int = 0 // The value is where the values will be in the new array, it is the position in the new array
    while(newArrayLocation != _capacity / 2) {
      newArray(newArrayLocation) = currentArray(currentPos)

      if(currentPos == (_capacity / 2) - 1) currentPos = 0
      else currentPos += 1
      newArrayLocation += 1
    }

    _lowerBound = 0
    _upperBound = newArrayLocation
    _dataArray = newArray
  }

  /**
   * Enqueues the provided element.
   * @param elem element to be enqueued.
   */
  override def enqueue(elem: A): Unit = {
    /**
     * The function first checks if the queue is at its capacity by comparing numStored to the capacity variable, if it is, reserve is called, which doubles
     * the capacity. If the queue is not at its capacity, we check if the upperbound is equal to the capacity, this would mean that there were values
     * removed from the front of the array through dequeue, in which case, the upperbound has to be set to 0, because the newest value would have to be set
     * at the front.
     *
     * The next conditional checks if the upperbound is equal to -1, if it is equal to -1, it sets both upper bound and lower bound to 0, because the only case
     * where it would equal -1 is if the queue is empty. So it can set both of the bound values equal to 0.
     *
     * Finally, the value at upperbound in the array is set to elem because the upperbound value is always the index of the last value in the queue + 1, and it doesn't
     * have to worry about the value stored at upperbound because it is properly updated previously. Afterwards numstored is incremented to match the added elem and
     * upperbound is incremented as well to match the invariant provided in the handout code.
     *
     * The upperbound invariant also allows us to easily find the position at which the elem should be put in the array
     *
     * The function is able to get away with setting the upperbound to 0 because upperbound is always incremented regardless of what it is set to at the end of the function.
     */
    if(_numStored == _capacity) {
      reserve()
    } else if(_upperBound == _capacity) {
      _upperBound = 0
    }

    if(_upperBound == -1) {
      _lowerBound = 0
      _upperBound = 0
    }

    _dataArray(_upperBound) = elem
    _upperBound += 1
    _numStored += 1
  }

  /** Returns the front of the queue. */
  override def front: A = {
    /**
     * The value at lower bound is always the front of the queue, so just calling that will give us the provided value, if the
     * lower bound is -1, then the queue is empty, so an exception is thrown.
     */
    if(_lowerBound == -1) throw new IllegalArgumentException("Front of empty queue")
    else _dataArray(_lowerBound)
  }

  /** Dequeues the appropriate element, if one exists. */
  override def dequeue(): A = {
    /**
     * First it checks if this queue is empty, and if it is an exception is thrown, we know the queue is empty when the queue
     * lowerbound is equal to -1
     *
     * Next we get the value located at the lowerbound index of the array, and that will be the value returned by the function.
     *
     * If lowerbound is located at capacity - 1, that means that the lowerbound value is at the very end of the array, so the next lowerbound
     * value would be at the very start of the array, because that would be the next value in the queue, other wise lowerbound can simply be
     * incremented, putting the value at the index of the next value of the queue.
     *
     * We remove 1 from _numStored to adjust the length of the queue, and then check if numStored has been set to 0, because if it is, the queue is now empty
     * so the lower and upper bounds must be set to -1.
     *
     */
    if(_lowerBound == -1) throw new IllegalArgumentException("Queue is empty")
    val returnValue: A = _dataArray(_lowerBound)

    if(_lowerBound == _capacity - 1) _lowerBound = 0
    else _lowerBound += 1

    _numStored -= 1

    if(_numStored == 0) {
      _lowerBound = -1
      _upperBound = -1
    }

    returnValue
  }
}