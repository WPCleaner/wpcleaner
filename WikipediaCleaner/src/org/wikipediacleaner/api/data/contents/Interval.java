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
  public int getBeginIndex();

  /**
   * @return End index of the interval.
   */
  public int getEndIndex();

  /**
   * @param index Index to check.
   * @return True if the provided index is inside the element.
   */
  public boolean containsIndex(int index);
}
