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
 * Algorithm for analyzing error 38 of check wikipedia project.
 * Error 38: HTML text style element &lt;i&gt;
 */
public class CheckErrorAlgorithm038 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm038() {
    super("HTML text style element <i>");
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
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieve all <i> tags
    List<PageElementTag> iTags = pageAnalysis.getTags(PageElementTag.TAG_HTML_I);
    boolean result = false;
    for (PageElementTag iTag : iTags) {

      // Check if the tag is an error
      boolean errorFound = false;
      if (iTag.isFullTag()) {
        errorFound = true;
      } else if (iTag.isEndTag()) {
        if (!iTag.isComplete()) {
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
        if (!iTag.isFullTag() && iTag.isComplete()) {
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(),
              iTag.getCompleteBeginIndex(),
              iTag.getCompleteEndIndex());
          String text = pageAnalysis.getContents().substring(
              iTag.getValueBeginIndex(),
              iTag.getValueEndIndex());
          if ((text != null) && (text.trim().length() > 0)) {
            String visibleText = text;
            if (text.length() > 30) {
              visibleText = text.substring(0, 10) + "…" + text.substring(text.length() - 10); 
            }
            error.addReplacement(
                "''" + text + "''",
                GT._("Replace with {0}", "''" + visibleText + "''"));
            error.addReplacement(
                text,
                GT._("Replace with {0}", visibleText));
          } else {
            error.addReplacement("", GT._("Delete"));
          }
          errors.add(error);
        } else {
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(), iTag.getBeginIndex(), iTag.getEndIndex());
          error.addReplacement("", GT._("Delete"));
          errors.add(error);
        }
      }
    }
    return result;
  }
}
