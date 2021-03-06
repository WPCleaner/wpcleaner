/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a03x.a038;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }
    if (Namespace.IMAGE == analysis.getPage().getNamespace()) {
      return false;
    }

    // Retrieve all <i> tags
    List<PageElementTag> iTags = analysis.getTags(HtmlTagType.I);
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

      // Check that error should be reported
      int beginIndex = iTag.getCompleteBeginIndex();
      if (errorFound) {
        if ((analysis.getSurroundingTag(WikiTagType.MATH, beginIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.NOWIKI, beginIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.PRE, beginIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.SOURCE, beginIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, beginIndex) != null)) {
          errorFound = false;
        }
      }

      // Mark error
      if (errorFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        if (!iTag.isFullTag() && iTag.isComplete()) {
          CheckErrorResult error = createCheckErrorResult(
              analysis,
              iTag.getCompleteBeginIndex(),
              iTag.getCompleteEndIndex());
          String text = analysis.getContents().substring(
              iTag.getValueBeginIndex(),
              iTag.getValueEndIndex());
          if ((text != null) && (text.trim().length() > 0)) {
            String visibleText = text;
            if (text.length() > 30) {
              visibleText = text.substring(0, 10) + "…" + text.substring(text.length() - 10); 
            }
            error.addReplacement(
                "''" + text + "''",
                GT._T("Replace with {0}", "''" + visibleText + "''"));
            error.addReplacement(
                text,
                GT._T("Replace with {0}", visibleText));
          } else {
            error.addReplacement("", GT._T("Delete"));
          }
          errors.add(error);
        } else {
          CheckErrorResult error = createCheckErrorResult(
              analysis, iTag.getBeginIndex(), iTag.getEndIndex());
          error.addReplacement("", GT._T("Delete"));
          errors.add(error);
        }
      }
    }
    return result;
  }
}
