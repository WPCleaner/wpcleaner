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
import org.wikipediacleaner.api.data.PageElementImage;


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
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (pageAnalysis == null) {
      return false;
    }

    // Check every image
    Collection<PageElementImage> images = pageAnalysis.getImages();
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
            pageAnalysis.getPage(),
            image.getBeginIndex(), image.getEndIndex());
        errors.add(error);
      }
    }

    return result;
  }
}
