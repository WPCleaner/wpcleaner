/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.algorithm;

import java.util.Comparator;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.configuration.CWConfigurationError;


/**
 * A comparator for CheckError objects by priority.
 */
public class AlgorithmErrorComparator implements Comparator<AlgorithmError> {

  /* (non-Javadoc)
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare(AlgorithmError o1, AlgorithmError o2) {

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

    // Check on priority
    CheckErrorAlgorithm a1 = o1.getAlgorithm();
    CheckErrorAlgorithm a2 = o2.getAlgorithm();
    int p1 = (a1 != null) ? a1.getPriority() : CWConfigurationError.PRIORITY_UNKOWN;
    int p2 = (a2 != null) ? a2.getPriority() : CWConfigurationError.PRIORITY_UNKOWN;
    if (p1 != p2) {
      return CWConfigurationError.comparePriority(p1, p2);
    }

    // Check on error number
    if (o1.getErrorNumber() != o2.getErrorNumber()) {
      return (o1.getErrorNumber() > o2.getErrorNumber()) ? 1 : -1;
    }

    return 0;
  }

}
