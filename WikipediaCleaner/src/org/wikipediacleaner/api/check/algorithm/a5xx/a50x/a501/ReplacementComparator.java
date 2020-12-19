/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a501;

import java.util.Comparator;

/**
 * Utility class to sort Replacement in a group.
 */
class ReplacementComparator implements Comparator<Replacement> {

  /**
   * Constructor.
   */
  public ReplacementComparator() {
  }

  /**
   * @param o1
   * @param o2
   * @return
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare(Replacement o1, Replacement o2) {
    
    // Comparison on native pattern
    if (o1.isOtherPattern() != o2.isOtherPattern()) {
      return (o1.isOtherPattern() ? 1 : -1);
    }

    // Comparison on automatic
    if (o1.isAutomatic() != o2.isAutomatic()) {
      return o1.isAutomatic() ? -1 : 1;
    }

    // Comparison on begin
    if (o1.getBegin() != o2.getBegin()) {
      return (o1.getBegin() < o2.getBegin() ? -1 : 1);
    }

    // Comparison on end
    if (o1.getEnd() != o2.getEnd()) {
      return (o1.getEnd() > o2.getEnd() ? -1 : 1);
    }

    // Comparison on comments
    if (o1.getComment() == null) {
      if (o2.getComment() != null) {
        return 1;
      }
    } else if (o2.getComment() == null) {
      return -1;
    } else {
      int compare = o1.getComment().compareTo(o2.getComment());
      if (compare != 0) {
        return compare;
      }
    }

    return 0;
  }
}
