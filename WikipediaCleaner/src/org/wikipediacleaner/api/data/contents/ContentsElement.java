/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import org.wikipediacleaner.api.data.analysis.Contents;

/**
 * Structured element inside contents.
 */
public abstract class ContentsElement implements Interval {

  /** Complete text of the element */
  private final String text;

  /** Interval on which the element is spanning inside the contents */
  private final ContentsInterval interval;

  /**
   * @param contents Contents of the page.
   * @param interval Interval on which the element is spanning.
   */
  ContentsElement(Contents contents, ContentsInterval interval) {
    this.text = contents.substring(interval);
    this.interval = interval;
  }

  /**
   * @param text Complete text of the element.
   * @param beginIndex Begin index of the element.
   * @param endIndex End index of the element.
   */
  @Deprecated
  // TODO: Change visibility to package, and remove
  public ContentsElement(String text, int beginIndex, int endIndex) {
    this.text = text;
    this.interval = new ContentsInterval(beginIndex, endIndex);
  }

  /**
   * @return Complete text of the element.
   */
  final public String getFullText() {
    return text;
  }

  /**
   * @return Interval on which the element is spanning.
   */
  final public ContentsInterval getInterval() {
    return interval;
  }

  /**
   * @return Begin index of the element.
   */
  @Override
  final public int getBeginIndex() {
    return interval.getBeginIndex();
  }

  /**
   * @return End index of the element.
   */
  @Override
  final public int getEndIndex() {
    return interval.getEndIndex();
  }

  /**
   * @param index Index to check.
   * @return True if the provided index is inside the element.
   */
  @Override
  final public boolean containsIndex(int index) {
    return interval.containsIndex(index);
  }

  /**
   * @return A string representation of the element.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    if (text != null) {
      return text;
    }
    return super.toString();
  }
}
