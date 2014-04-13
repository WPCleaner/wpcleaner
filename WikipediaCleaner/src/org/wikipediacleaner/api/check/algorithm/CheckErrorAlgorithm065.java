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
 * Algorithm for analyzing error 65 of check wikipedia project.
 * Error 65: Image description with break
 */
public class CheckErrorAlgorithm065 extends CheckErrorAlgorithmBase {

  private final static String[] possibleBreaks = { "<br>", "<br/>", "</br>", "<br />" };

  public CheckErrorAlgorithm065() {
    super("Image description with break");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
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
      String description = image.getDescription();
      if (description != null) {
        boolean breakFound = false;
        boolean shouldStop = false;
        while ((description.length() > 0) && (!shouldStop)) {
          shouldStop = true;
          while ((description.length() > 0) &&
                 (Character.isWhitespace(description.charAt(description.length() - 1)))) {
            description = description.substring(0, description.length() - 1);
          }
          for (String possibleBreak : possibleBreaks) {
            if (description.endsWith(possibleBreak)) {
              breakFound = true;
              shouldStop = false;
              description = description.substring(
                  0, description.length() - possibleBreak.length());
            }
          }
        }
        if (breakFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              image.getBeginIndex(),
              image.getEndIndex());
          errorResult.addReplacement(image.getDescriptionReplacement(description));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
