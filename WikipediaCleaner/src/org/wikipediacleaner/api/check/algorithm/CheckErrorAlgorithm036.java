/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 36 of check wikipedia project.
 * Error 36: Redirect not correct
 */
public class CheckErrorAlgorithm036 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm036() {
    super("Redirect not correct");
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

    // Retrieve magic word for redirects
    MagicWord redirect = analysis.getWikiConfiguration().getMagicWordByName(MagicWord.REDIRECT);
    if ((redirect == null) ||
        (redirect.getAliases() == null) ||
        (redirect.getAliases().isEmpty())) {
      return false;
    }

    // Analysis of all article text
    boolean result = false;
    String contents = analysis.getContents();
    int currentIndex = 0;
    while (currentIndex < contents.length()) {

      // Check if we are at the beginning of a #REDIRECT
      String aliasFound = null;
      if (contents.charAt(currentIndex) == '#') {
        for (String alias : redirect.getAliases()) {
          int endIndex = currentIndex + alias.length();
          if ((endIndex < contents.length()) &&
              (alias.length() > 0) &&
              (alias.equalsIgnoreCase(contents.substring(currentIndex, endIndex)))) {
            char nextChar = contents.charAt(endIndex);
            if ((nextChar == ' ') || (nextChar == '[')) {
              aliasFound = alias;
            }
          }
        }
      }

      // Check if there's a link after the #REDIRECT
      boolean linkFound = false;
      if (aliasFound != null) {
        int tmpIndex = currentIndex + aliasFound.length();
        while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '[')) {
          PageElementInternalLink link = analysis.isInInternalLink(tmpIndex);
          if ((link != null) && (link.getBeginIndex() == tmpIndex)) {
            linkFound = true;
          }
          if (!linkFound) {
            PageElementCategory category = analysis.isInCategory(tmpIndex);
            if ((category != null) && (category.getBeginIndex() == tmpIndex)) {
              linkFound = true;
            }
          }
        }
      }

      // Report error if needed
      if (aliasFound != null) {
        if (!linkFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, currentIndex, currentIndex + aliasFound.length());
          errors.add(errorResult);
        }
        currentIndex += aliasFound.length();
      } else {
        currentIndex++;
      }
    }

    return result;
  }
}
