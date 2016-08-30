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
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 19 of check wikipedia project.
 * Error 19: Headlines start with one "="
 */
public class CheckErrorAlgorithm019 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm019() {
    super("Headlines start with one \"=\"");
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
    boolean result = false;
    List<PageElementTitle> titles = analysis.getTitles();
    for (PageElementTitle title : titles) {
      if (title.getLevel() == 1) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            title.getBeginIndex(), title.getEndIndex());
        errorResult.addEditTocAction(title);
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    String contents = analysis.getContents();
    if (!analysis.areTitlesReliable()) {
      return contents;
    }

    // Compute minimum title level
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || (titles.size() == 0)) {
      return contents;
    }
    if ((titles.get(0).getLevel() > 1) ||
        (!titles.get(0).isCoherent())) {
      return contents;
    }
    int minTitle = Integer.MAX_VALUE;
    for (PageElementTitle title : titles) {
      if (title.getLevel() < minTitle) {
        minTitle = title.getLevel();
      }
    }
    if (minTitle > 1) {
      return contents;
    }

    // Replace titles
    StringBuilder tmp = new StringBuilder();
    int lastIndex = 0;
    boolean found = false;
    for (PageElementTitle title : titles) {
      if (!found && title.getLevel() == 1) {
        found = true;
      }
      if (found) {
        if (lastIndex < title.getBeginIndex()) {
          tmp.append(contents.substring(lastIndex, title.getBeginIndex()));
          lastIndex = title.getBeginIndex();
        }
        tmp.append(PageElementTitle.createTitle(
            title.getLevel() + 1, title.getTitle(), title.getAfterTitle()));
        if (title.getAfterTitle() != null) {
          tmp.append(title.getAfterTitle());
        }
        lastIndex = title.getEndIndex();
      }
    }
    if (lastIndex < contents.length()) {
      tmp.append(contents.substring(lastIndex));
    }

    return tmp.toString();
  }
}
