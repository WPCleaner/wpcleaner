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
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


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
  @Override
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

  /**
   * Prefixes to look for.
   */
  private final static String[] prefixes = {
    "www."
  };

  /**
   * Analyze an area for finding URL without http://
   * 
   * @param analysis Page analysis.
   * @param beginIndex Begin index of the area to analyze.
   * @param endIndex End index of the area to analyze.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
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
          boolean shouldCount = true;
          int currentIndex = beginIndex + index;
          if (shouldCount) {
            // Check for external link
            PageElementExternalLink link = analysis.isInExternalLink(currentIndex);
            if (link != null) {
              shouldCount = false;
            }
          }
          if (shouldCount) {
            // Check for comment
            if (analysis.comments().isAt(currentIndex)) {
              shouldCount = false;
            }
          }
          if (shouldCount) {
            // Check for template
            PageElementTemplate template = analysis.isInTemplate(currentIndex);
            if (template != null) {
              for (int numParam = 0; numParam < template.getParameterCount(); numParam++) {
                if (template.getParameterValueStartIndex(numParam) == currentIndex) {
                  shouldCount = false;
                }
              }
            }
          }
          if (shouldCount) {
            // Check characters before
            int tmpIndex = index - 1;
            boolean done = false;
            while (shouldCount && !done && (tmpIndex > 0)) {
              char currentChar = text.charAt(tmpIndex);
              if ((currentChar == '/') && (text.charAt(tmpIndex - 1) == '/')) {
                done = true;
                shouldCount = false;
              } else {
                if (!Character.isLetter(currentChar) &&
                    !Character.isDigit(currentChar) &&
                    ("/_-.".indexOf(currentChar) < 0)) {
                  done = true;
                }
              }
              tmpIndex--;
            }
          }
          if (shouldCount) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                currentIndex, currentIndex + prefix.length());
            errorResult.addReplacement(
                "http://" + analysis.getContents().substring(currentIndex, currentIndex + prefix.length()),
                GT._T("Add {0}", "http://"));
            errorResult.addReplacement("", GT._T("Remove {0}", prefix));
            errors.add(errorResult);
          }
        }
      }
      index = nextIndex;
    }
    return result;
  }
}
