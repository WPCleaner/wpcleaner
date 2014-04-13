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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 26 of check wikipedia project.
 * Error 26: HTML text style element &lt;b&gt;
 */
public class CheckErrorAlgorithm026 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm026() {
    super("HTML text style element <b>");
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

    // Retrieve all <b> tags
    List<PageElementTag> bTags = analysis.getTags(PageElementTag.TAG_HTML_B);
    boolean result = false;
    for (PageElementTag bTag : bTags) {

      // Check if the tag is an error
      boolean errorFound = false;
      if (bTag.isFullTag()) {
        errorFound = true;
      } else if (bTag.isEndTag()) {
        if (!bTag.isComplete()) {
          errorFound = true;
        }
      } else {
        errorFound = true;
      }

      // Mark error
      if (errorFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        if (!bTag.isFullTag() && bTag.isComplete()) {
          CheckErrorResult error = createCheckErrorResult(
              analysis.getPage(),
              bTag.getCompleteBeginIndex(),
              bTag.getCompleteEndIndex());
          String text = analysis.getContents().substring(
              bTag.getValueBeginIndex(),
              bTag.getValueEndIndex());
          if ((text != null) && (text.trim().length() > 0)) {
            String visibleText = text;
            if (text.length() > 30) {
              visibleText = text.substring(0, 10) + "…" + text.substring(text.length() - 10); 
            }
            error.addReplacement(
                "'''" + text + "'''",
                GT._("Replace with {0}", "'''" + visibleText + "'''"));
            error.addReplacement(
                text,
                GT._("Replace with {0}", visibleText));
          } else {
            error.addReplacement("", GT._("Delete"));
          }
          errors.add(error);
        } else {
          CheckErrorResult error = createCheckErrorResult(
              analysis.getPage(), bTag.getBeginIndex(), bTag.getEndIndex());
          error.addReplacement("", GT._("Delete"));
          errors.add(error);
        }
      }
    }
    return result;
  }
}
