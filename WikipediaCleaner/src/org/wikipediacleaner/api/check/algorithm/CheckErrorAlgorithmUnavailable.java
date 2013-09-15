/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;


/**
 * Base class for unavailable errors.
 */
public abstract class CheckErrorAlgorithmUnavailable extends CheckErrorAlgorithmBase {

  /**
   * @param shortDescription
   */
  public CheckErrorAlgorithmUnavailable(String shortDescription) {
    super(shortDescription);

  }

  /**
   * @return Flag indicating if this algorithm is available.
   */
  @Override
  public boolean isAvailable() {
    return false;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    return false;
  }
}