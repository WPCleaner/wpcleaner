/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents;

import java.util.Comparator;


/**
 * Comparator for PageElement.
 */
public class IntervalComparator implements Comparator<Interval> {

  @Override
  public int compare(Interval o1, Interval o2) {
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
