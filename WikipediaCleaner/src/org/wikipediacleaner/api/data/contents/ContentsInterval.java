/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;


/**
 * A bean for handling intervals of contents.
 */
public class ContentsInterval {

  /** Begin index of the interval */
  private final int beginIndex;

  /** End index of the interval */
  private final int endIndex;

  /**
   * @param beginIndex Begin index of the interval.
   * @param endIndex End index of the interval.
   */
  public ContentsInterval(int beginIndex, int endIndex) {
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
  }

  /**
   * @return Begin index of the interval.
   */
  public int getBeginIndex() {
    return beginIndex;
  }

  /**
   * @return End index of the interval.
   */
  public int getEndIndex() {
    return endIndex;
  }

  /**
   * @param index Index to check.
   * @return True if the provided index is inside the interval.
   */
  public boolean containsIndex(int index) {
    if ((index >= beginIndex) && (index < endIndex)) {
      return true;
    }
    return false;
  }
}
