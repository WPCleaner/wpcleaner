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
 * Algorithm for analyzing error 93 of check wikipedia project.
 * Error 93: External link with double http://
 */
public class CheckErrorAlgorithm093 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm093() {
    super("External link with double http://");
  }

  private static String[] possiblePrefixes = {
    "http://",  "http:/",  "http:",
    "https://", "https:/", "https:",
  };

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

    // Analyze each external link
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {

      // Initialization
      boolean errorFound = false;
      int beginIndex = link.getBeginIndex();
      String dest = link.getLink();
      String prefix1 = null;
      String prefix2 = null;
      String withoutPrefix = null;

      // Check if there's a double prefix inside the link
      if (!errorFound) {
        for (String tmp1 : possiblePrefixes) {
          if ((prefix1 == null) &&
              (tmp1.length() < dest.length()) &&
              (tmp1.equalsIgnoreCase(dest.substring(0, tmp1.length())))) {
            for (String tmp2 : possiblePrefixes) {
              if ((prefix2 == null) &&
                  (tmp1.length() + tmp2.length() < dest.length()) &&
                  tmp2.equalsIgnoreCase(dest.substring(
                      tmp1.length(),
                      tmp1.length() + tmp2.length()))) {
                errorFound = true;
                prefix1 = tmp1;
                prefix2 = tmp2;
                withoutPrefix = dest.substring(tmp1.length() + tmp2.length());
              }
            }
          }
        }

        // Check if there's a second prefix before the link
        if (!errorFound && !link.hasSquare()) {
          for (String tmp2 : possiblePrefixes) {
            if ((prefix2 == null) &&
                (tmp2.length() < dest.length()) &&
                (tmp2.equalsIgnoreCase(dest.substring(0, tmp2.length())))) {
              prefix2 = tmp2;
            }
          }
          for (String tmp1 : possiblePrefixes) {
            if ((prefix1 == null) &&
                (beginIndex >= tmp1.length()) &&
                (tmp1.equalsIgnoreCase(contents.substring(beginIndex - tmp1.length(), beginIndex)))) {
              prefix1 = tmp1;
            }
          }
          if ((prefix1 != null) && (prefix2 != null)) {
            errorFound = true;
            beginIndex -= prefix1.length();
            withoutPrefix = dest.substring(prefix2.length());
          }
        }

        // Raise error
        if (errorFound && (prefix1 != null) && (prefix2 != null)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, link.getEndIndex());
          if (prefix2.endsWith("//")) {
            String newDest = prefix2 + withoutPrefix;
            if (link.hasSquare()) {
              errorResult.addReplacement(PageElementExternalLink.createExternalLink(
                  newDest, link.getText()));
            } else {
              errorResult.addReplacement(newDest);
            }
          }
          if (!prefix1.equalsIgnoreCase(prefix2) &&
              (prefix1.endsWith("//"))) {
            String newDest = prefix1 + withoutPrefix;
            if (link.hasSquare()) {
              errorResult.addReplacement(PageElementExternalLink.createExternalLink(
                  newDest, link.getText()));
            } else {
              errorResult.addReplacement(newDest);
            }
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
