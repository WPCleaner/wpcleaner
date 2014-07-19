/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.util.Comparator;


/**
 * Comparator for algorithms.
 */
public class CheckErrorAlgorithmComparator implements
    Comparator<CheckErrorAlgorithm> {

  /**
   * @param o1
   * @param o2
   * @return
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare(CheckErrorAlgorithm o1, CheckErrorAlgorithm o2) {

    // Check on null values
    if (o1 == null) {
      if (o2 == null) {
        return 0;
      }
      return -1;
    }
    if (o2 == null) {
      return 1;
    }

    // Check on error number
    if (o1.getErrorNumber() != o2.getErrorNumber()) {
      return (o1.getErrorNumber() > o2.getErrorNumber()) ? 1 : -1;
    }

    return 0;
  }

}
