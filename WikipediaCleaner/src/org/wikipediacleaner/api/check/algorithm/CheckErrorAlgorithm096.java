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
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 96 of check wikipedia project.
 * Error 96: TOC after first headline
 */
public class CheckErrorAlgorithm096 extends CheckErrorAlgorithmTOC {

  public CheckErrorAlgorithm096() {
    super("TOC after first headline");
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

    // Initialization
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || (titles.isEmpty())) {
      return false;
    }
    PageElementTitle firstTitle = titles.get(0);
    List<PageElement> tocs = getToCs(analysis, firstTitle.getEndIndex(), Integer.MAX_VALUE);
    if ((tocs == null) || (tocs.isEmpty())) {
      return false;
    }

    // Check every table of contents
    boolean result = false;
    for (PageElement toc : tocs) {
      if (toc.getBeginIndex() >= firstTitle.getEndIndex()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            toc.getBeginIndex(), toc.getEndIndex());
        errors.add(errorResult);
      }
    }

    return result;
  }
}
