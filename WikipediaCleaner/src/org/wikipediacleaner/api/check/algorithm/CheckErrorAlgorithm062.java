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
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 62 of check wikipedia project.
 * Error 62: URL containing no http://
 */
public class CheckErrorAlgorithm062 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm062() {
    super("URL containing no http://");
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

    // Analyze every reference
    boolean result = false;
    List<PageElementTag> refs = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    for (PageElementTag ref : refs) {
      if (!ref.isFullTag() && ref.isComplete()) {
        result |= analyzeArea(
            analysis, ref.getValueBeginIndex(), ref.getValueEndIndex(),
            errors);
        if ((errors == null) && result) {
          return true;
        }
      }
    }

    return result;
  }

  private final static String[] prefixes = {
    "www."
  };

  public boolean analyzeArea(
      PageAnalysis analysis, int beginIndex, int endIndex,
      Collection<CheckErrorResult> errors) {
    boolean result = false;
    String text = analysis.getContents().substring(beginIndex, endIndex);
    int index = 0;
    while (index < text.length()) {
      int nextIndex = index + 1;
      for (String prefix : prefixes) {
        if (text.startsWith(prefix, index) &&
            (index + prefix.length() < text.length()) &&
            (Character.isLetterOrDigit(text.charAt(index + prefix.length())))) {
          PageElementExternalLink link = analysis.isInExternalLink(beginIndex + index);
          if (link == null) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis.getPage(),
                beginIndex + index, beginIndex + index + prefix.length());
            errors.add(errorResult);
          }
        }
      }
      index = nextIndex;
    }
    return result;
  }
}
