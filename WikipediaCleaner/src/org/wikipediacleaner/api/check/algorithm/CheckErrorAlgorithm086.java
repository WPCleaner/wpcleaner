/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;


/**
 * Algorithm for analyzing error 86 of check wikipedia project.
 * Error 86: External link with two brackets
 */
public class CheckErrorAlgorithm086 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm086() {
    super("External link with two brackets");
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

    // For each external link
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    String contents = analysis.getContents();
    int maxLength = contents.length();
    boolean result = false;
    for (PageElementExternalLink link : links) {
      if (link.hasSquare() && link.hasSecondSquare()) {
        int beginIndex = link.getBeginIndex() - 1;
        if ((beginIndex >= 0) &&
            (contents.charAt(beginIndex) == '[')) {
          int endIndex = link.getEndIndex();
          if ((endIndex < maxLength) && (contents.charAt(endIndex) == ']')) {
            endIndex++;
          }
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
          String externalLink = contents.substring(link.getBeginIndex(), link.getEndIndex());
          errorResult.addReplacement(externalLink);
          errorResult.addReplacement(externalLink.replaceAll("\\|", " "));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
