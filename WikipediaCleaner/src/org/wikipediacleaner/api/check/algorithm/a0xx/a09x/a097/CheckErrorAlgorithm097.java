/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a09x.a097;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTOC;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 97 of check wikipedia project.
 * Error 97: Material between TOC and first headline
 */
public class CheckErrorAlgorithm097 extends CheckErrorAlgorithmTOC {

  public CheckErrorAlgorithm097() {
    super("Material between TOC and first headline");
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
    int maxIndex = analysis.getContents().length();
    if ((titles != null) && (!titles.isEmpty())) {
      maxIndex = titles.get(0).getBeginIndex();
    }
    List<PageElement> tocs = getToCs(analysis, 0, maxIndex);
    if ((tocs == null) || (tocs.isEmpty())) {
      return false;
    }

    // Check every table of contents
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElement toc : tocs) {
      if (toc.getBeginIndex() < maxIndex) {
        boolean extraChar = false;
        int index = toc.getEndIndex();
        while (index < maxIndex) {
          if (!Character.isWhitespace(contents.charAt(index))) {
            extraChar = true;
          }
          index++;
        }
        if (extraChar) {
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
    }

    return result;
  }
}
