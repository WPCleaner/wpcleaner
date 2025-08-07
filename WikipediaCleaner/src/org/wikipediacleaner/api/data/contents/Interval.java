/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;


/**
 * An interface for interval of contents.
 */
public interface Interval {

  /**
   * @return Begin index of the interval.
   */
  int getBeginIndex();

  /**
   * @return End index of the interval.
   */
  int getEndIndex();

  /**
   * @param index Index to check.
   * @return True if the provided index is inside the element.
   */
  default boolean containsIndex(final int index) {
    return (index >= getBeginIndex() && index < getEndIndex());
  }

  default boolean overlap(final int beginIndex, final int endIndex) {
    return (beginIndex < getEndIndex() && endIndex > getBeginIndex());
  }
}
