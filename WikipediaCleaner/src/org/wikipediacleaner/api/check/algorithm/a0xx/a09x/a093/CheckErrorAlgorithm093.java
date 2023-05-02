/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a09x.a093;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


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
    for (PageElementExternalLink link : links) {
      result |= analyzeExternalLink(analysis, errors, link);
    }

    return result;
  }

  /**
   * Analyze an external to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param link External link to be analyzed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeExternalLink(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementExternalLink link) {

    // Initialization
    int beginIndex = link.getBeginIndex();
    String dest = link.getLink();

    // Check if there's a double prefix inside the link
    boolean errorFound = false;
    String prefix1 = null;
    String prefix2 = null;
    String withoutPrefix = null;
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
      String contents = analysis.getContents();
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

    if (!errorFound || (prefix1 == null) || (prefix2 == null)) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Raise error
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, link.getEndIndex());
    if (prefix1.equalsIgnoreCase(prefix2)) {
      addReplacement(
          errorResult,
          link,
          prefix1.toLowerCase(Locale.ROOT) + withoutPrefix,
          prefix1.endsWith("//"));
    } else {
      if (prefix2.endsWith("//")) {
        addReplacement(
            errorResult,
            link,
            prefix2.toLowerCase(Locale.ROOT) + withoutPrefix,
            prefix2.equalsIgnoreCase("https://") && prefix1.equalsIgnoreCase("http://"));
      }
      if (prefix1.endsWith("//")) {
        addReplacement(
            errorResult,
            link,
            prefix1.toLowerCase(Locale.ROOT) + withoutPrefix,
            prefix1.equalsIgnoreCase("https://") && prefix2.equalsIgnoreCase("http://"));
      }
    }
    errors.add(errorResult);
    return true;
  }

  private void addReplacement(
      CheckErrorResult errorResult, PageElementExternalLink link,
      String dest, boolean automatic) {
    if (link.hasSquare()) {
      errorResult.addReplacement(
          PageElementExternalLink.createExternalLink(dest, link.getText()),
          automatic);
    } else {
      errorResult.addReplacement(dest, automatic);
    }
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
