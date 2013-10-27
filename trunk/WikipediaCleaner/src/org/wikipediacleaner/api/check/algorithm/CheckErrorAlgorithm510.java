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
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 510 of check wikipedia project.
 * Error 510: Non working pipe trick
 */
public class CheckErrorAlgorithm510 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm510() {
    super("Non working pipe trick");
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
    if ((pageAnalysis == null) || (pageAnalysis.getInternalLinks() == null)) {
      return false;
    }

    // Analyze each internal link
    boolean result = false;
    List<PageElementInternalLink> links = pageAnalysis.getInternalLinks();
    for (PageElementInternalLink link : links) {
      if ((link.getText() != null) &&
          (link.getText().length() == 0) &&
          (link.getFullLink() != null)) {
        boolean errorFound = false;
        String target = link.getFullLink().trim();
        if (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, link.getBeginIndex()) != null) {

          // Check for namespace at the beginning
          int beginIndex = 0;
          if (target.length() > 1) {
            int tmpIndex = target.indexOf(':');
            if (tmpIndex > 0) {
              beginIndex = tmpIndex + 1;
            }
          }

          // Check for parenthesis or commas (remove the end part)
          int endIndex = target.length();
          if (endIndex > 0) {
            if (target.charAt(endIndex - 1) == ')') {
              int tmpIndex = target.lastIndexOf('(');
              if (tmpIndex > 0) {
                endIndex = tmpIndex;
              }
            }
            if (endIndex == target.length()) {
              int tmpIndex = target.indexOf(',');
              if (tmpIndex > 0) {
                endIndex = tmpIndex;
              }
            }
          }

          // Report error
          if ((beginIndex > 0) || (endIndex < target.length())) {
            if (errors == null) {
              return true;
            }
            result = true;
            errorFound = true;
            if (beginIndex >= endIndex) {
              beginIndex = 0;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
            String replacement = PageElementInternalLink.createInternalLink(
                target,
                target.substring(beginIndex, endIndex));
            errorResult.addReplacement(replacement);
            replacement = PageElementInternalLink.createInternalLink(target, null);
            errorResult.addReplacement(replacement);
            errors.add(errorResult);
          }
        }

        // Incorrect slash trick
        if (!errorFound) {
          int endIndex = target.length();
          if ((endIndex > 1) && (target.charAt(0) == '/')) {
            if (errors == null) {
              return true;
            }
            result = true;
            errorFound = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
            String replacement = PageElementInternalLink.createInternalLink(
                target + "/", null);
            errorResult.addReplacement(replacement);
            replacement = PageElementInternalLink.createInternalLink(target, null);
            errorResult.addReplacement(replacement);
            errors.add(errorResult);
          }
        }

        // Link to section
        if (!errorFound) {
          int index = target.indexOf('#');
          if ((index >= 0) && (index < target.length() - 1)) {
            if (errors == null) {
              return true;
            }
            result = true;
            errorFound = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
            String replacement = PageElementInternalLink.createInternalLink(
                target, target.substring(index + 1));
            errorResult.addReplacement(replacement);
            replacement = PageElementInternalLink.createInternalLink(target, null);
            errorResult.addReplacement(replacement);
            errors.add(errorResult);
          }
        }
      }
    }
    return result;
  }
}
