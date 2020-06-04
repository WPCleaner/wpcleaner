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
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 552 of check wikipedia project.
 * Error 552: Template ending with }}}.
 */
public class CheckErrorAlgorithm552 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm552() {
    super("Template ending with }}}");
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

    // Check each template
    List<PageElementTemplate> templates = analysis.getTemplates();
    if (templates == null) {
      return false;
    }
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTemplate template : templates) {
      int endIndex = template.getEndIndex();
      if ((endIndex < contents.length()) && (contents.charAt(endIndex) == '}')) {

        // Check if there's something explaining this extra bracket
        boolean foundReason = false;
        if (!foundReason) {
          PageElementTemplate otherTemplate = analysis.isInTemplate(endIndex);
          if ((otherTemplate != null) &&
              (otherTemplate.getEndIndex() == endIndex + 2)) {
            foundReason = true;
          }
        }

        // Report error
        if (!foundReason) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, endIndex, endIndex + 1);
          errorResult.addReplacement("");
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
