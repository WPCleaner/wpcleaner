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
import org.wikipediacleaner.api.data.PageElementInterwikiLink;


/**
 * Algorithm for analyzing error 82 of check wikipedia project.
 * Error 82: Link to other wikiproject
 */
public class CheckErrorAlgorithm082 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm082() {
    super("Link to other wikiproject");
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
    if (pageAnalysis == null) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = -1;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      PageElementInterwikiLink link = pageAnalysis.getNextInterwikiLink(startIndex);
      if (link != null) {
        startIndex = link.getEndIndex();
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
        errors.add(errorResult);
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }
}