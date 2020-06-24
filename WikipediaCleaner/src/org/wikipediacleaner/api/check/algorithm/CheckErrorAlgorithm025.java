/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Vector;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 25 of check wikipedia project.
 * Error 25: Headline hierarchy
 */
public class CheckErrorAlgorithm025 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm025() {
    super("Headline hierarchy");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Check every title
    List<PageElementTitle> titles = analysis.getTitles();
    boolean result = false;
    int previousTitleLevel = -1;
    for (PageElementTitle title : titles) {
      if ((previousTitleLevel > 0) &&
          (title.getLevel() > previousTitleLevel + 1)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, title.getBeginIndex(), title.getEndIndex());
        errorResult.addEditTocAction(title);
        errors.add(errorResult);
      }
      previousTitleLevel = title.getLevel();
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

    // Replace titles
    StringBuilder tmp = new StringBuilder();
    int lastIndex = 0;
    Vector<Integer> offsets = new Vector<Integer>();
    List<PageElementTitle> titles = analysis.getTitles();
    for (int index = 0; index < titles.size(); index++) {

      // Compute current offset
      PageElementTitle title = titles.get(index);
      offsets.setSize(title.getLevel());
      int offset = 0;
      for (Integer levelOffset : offsets) {
        if (levelOffset != null) {
          offset += levelOffset.intValue();
        }
      }

      // Replace title if needed
      if (offset > 0) {
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

      // Compute level offset
      int levelOffset = Integer.MAX_VALUE;
      for (int index2 = index + 1;
           (index2 < titles.size()) && (titles.get(index2).getLevel() > title.getLevel());
           index2++) {
        levelOffset = Math.min(
            levelOffset,
            titles.get(index2).getLevel() - title.getLevel() - 1);
      }
      offsets.add(Integer.valueOf(levelOffset));
      if ((levelOffset == 0) &&
          (index + 1 < titles.size()) &&
          (titles.get(index + 1).getLevel() > title.getLevel() + 1)) {
        offsets.add(Integer.valueOf(titles.get(index + 1).getLevel() - title.getLevel() - 1));
      }
    }
    if (lastIndex == 0) {
      return contents;
    }
    if (lastIndex < contents.length()) {
      tmp.append(contents.substring(lastIndex));
    }

    return tmp.toString();
  }
}
