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
 * Algorithm for analyzing error 543 of check wikipedia project.
 * Error 543: Pipe in external link.
 */
public class CheckErrorAlgorithm543 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm543() {
    super("Pipe in external link");
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

    // Analyze each external link
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {

      // Check if the link target contains a pipe
      boolean hasPipe = false;
      String target = link.getLink();
      int pipeIndex = target.indexOf('|');
      if (pipeIndex > 0) {
        hasPipe = true;
      }

      // Check if the link target has a pipe just after
      if (!hasPipe && link.hasSquare()) {
        int index = link.getBeginIndex() + 1 + target.length();
        if ((index < contents.length()) && (contents.charAt(index) == '|')) {
          hasPipe = true;
        }
      }

      // Report error
      if (hasPipe) {
        if (errors == null) {
          return true;
        }
        result = true;

        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        CheckErrorResult error = createCheckErrorResult(analysis, beginIndex, endIndex);
        errors.add(error);
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
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
