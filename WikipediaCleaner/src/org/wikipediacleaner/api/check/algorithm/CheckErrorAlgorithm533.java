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


/**
 * Algorithm for analyzing error 533 of check wikipedia project.
 * Error 533: Multi colon escape (see [[Special:LintErrors/multi-colon-escape]])
 */
public class CheckErrorAlgorithm533 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm533() {
    super("Multi colon escape");
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

    // Analyze each internal link
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementInternalLink link : links) {
      String target = link.getLink();
      if ((target != null) && (target.startsWith("::"))) {
        result = true;
        if (errors == null) {
          return result;
        }
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        int tmpIndex = beginIndex + 2;
        StringBuilder buffer = new StringBuilder();
        buffer.append(contents.substring(beginIndex, tmpIndex));
        while ((tmpIndex < endIndex) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        while ((tmpIndex < endIndex) && (contents.charAt(tmpIndex) == ':')) {
          tmpIndex++;
        }
        buffer.append(':');
        buffer.append(contents.substring(tmpIndex, endIndex));
        CheckErrorResult error = createCheckErrorResult(analysis, beginIndex, endIndex);
        error.addReplacement(buffer.toString(), true);
        errors.add(error);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
