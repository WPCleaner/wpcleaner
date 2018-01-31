/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.Comparator;


/**
 * Comparator for elements of contents.
 */
public class ContentsElementComparator implements Comparator<ContentsElement> {

  /**
   * @param o1 First element.
   * @param o2 Second element.
   * @return Negative integer if first element is before second element.
   *         Positive integer if first element is after second element.
   *         Zero if both elements are at the same place.
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare(ContentsElement o1, ContentsElement o2) {
    if (o1 == null) {
      if (o2 == null) {
        return 0;
      }
      return -1;
    }
    if (o2 == null) {
      return 1;
    }
    if (o1.getBeginIndex() != o2.getBeginIndex()) {
      return (o1.getBeginIndex() < o2.getBeginIndex()) ? -1 : 1;
    }
    if (o1.getEndIndex() != o2.getEndIndex()) {
      return (o1.getEndIndex() < o2.getEndIndex()) ? -1 : 1;
    }
    return 0;
  }

}
