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
 * Algorithm for analyzing error 7 of check wikipedia project.
 * Error 7: Headlines all start with three "="
 */
public class CheckErrorAlgorithm007 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm007() {
    super("Headlines all start with three \"=\"");
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

    // Check level of each title
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || (titles.size() == 0)) {
      return false;
    }
    for (PageElementTitle title : titles) {
      if (title.getLevel() < 3) {
        return false;
      }
    }

    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        titles.get(0).getBeginIndex(), titles.get(0).getEndIndex());
    if (titles.size() == 1) {
      errorResult.addReplacement(PageElementTitle.createTitle(
          2, titles.get(0).getTitle(), titles.get(0).getAfterTitle()));
    }
    errorResult.addEditTocAction(titles.get(0));
    errors.add(errorResult);
    return true;
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
    int minTitle = Integer.MAX_VALUE;
    for (PageElementTitle title : titles) {
      if (title.getLevel() < minTitle) {
        minTitle = title.getLevel();
      }
    }
    if (minTitle < 3) {
      return contents;
    }

    // Replace titles
    StringBuilder tmp = new StringBuilder();
    int lastIndex = 0;
    int offset = minTitle - 2;
    for (PageElementTitle title : titles) {
      if (lastIndex < title.getBeginIndex()) {
        tmp.append(contents.substring(lastIndex, title.getBeginIndex()));
        lastIndex = title.getBeginIndex();
      }
      tmp.append(PageElementTitle.createTitle(
          title.getLevel() - offset, title.getTitle(), title.getAfterTitle()));
      if (title.getAfterTitle() != null) {
        tmp.append(title.getAfterTitle());
      }
      lastIndex = title.getEndIndex();
    }
    if (lastIndex < contents.length()) {
      tmp.append(contents.substring(lastIndex));
    }

    return tmp.toString();
  }
}
