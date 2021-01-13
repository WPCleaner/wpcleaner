/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a505;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 505 of check wikipedia project.
 * Error 505: Image without alternative description
 */
public class CheckErrorAlgorithm505 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm505() {
    super("Image without alternative description");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check every image
    Collection<PageElementImage> images = analysis.getImages();
    if ((images == null) || (images.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementImage image : images) {
      String alt = image.getAlternateDescription();
      if ((alt == null) || (alt.trim().length() == 0)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult error = createCheckErrorResult(
            analysis,
            image.getBeginIndex(), image.getEndIndex());
        errors.add(error);
      }
    }

    return result;
  }
}
