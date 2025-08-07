/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;


/**
 * Abstract decorator class for Interval.
 */
public abstract class IntervalDecorator implements Interval {

  private final Interval interval;

  /**
   * Constructor.
   * 
   * @param interval Inner interval.
   */
  public IntervalDecorator(Interval interval) {
    this.interval = interval;
  }

  /**
   * @return Begin index of the interval.
   * @see org.wikipediacleaner.api.data.contents.Interval#getBeginIndex()
   */
  @Override
  public int getBeginIndex() {
    return (interval != null) ? interval.getBeginIndex() : 0;
  }

  /**
   * @return End index of the interval.
   * @see org.wikipediacleaner.api.data.contents.Interval#getEndIndex()
   */
  @Override
  public int getEndIndex() {
    return (interval != null) ? interval.getEndIndex() : 0;
  }
}
