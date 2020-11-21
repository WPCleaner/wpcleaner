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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;


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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyze each title
    boolean result = false;
    List<PageElementTitle> titles = analysis.getTitles();
    for (int titleIndex = 0; titleIndex < titles.size(); titleIndex++) {
      PageElementTitle title = titles.get(titleIndex);
      if (title.getLevel() == 1) {
        if (errors == null) {
          return true;
        }
        boolean firstDetection = !result;
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            title.getBeginIndex(), title.getEndIndex());
        if (firstDetection) {
          if (titleIndex == titles.size() - 1) {
            errorResult.addReplacement(
                TitleBuilder.from(2, title.getTitle()).withAfter(title.getAfterTitle()).toString(),
                true);
          } else if ((titleIndex == 0) &&
              Page.areSameTitle(analysis.getPage().getTitle(), title.getTitle())) {
            errorResult.addReplacement(title.getAfterTitle(), true);
          }
        }
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
    if (!analysis.getPage().isInMainNamespace() ||
        !analysis.getPage().isArticle()) {
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
    int minTitleCount = 0;
    for (PageElementTitle title : titles) {
      if (title.getLevel() < minTitle) {
        minTitle = title.getLevel();
        minTitleCount = 1;
      } else if (title.getLevel() == minTitle) {
        minTitleCount++;
      }
    }
    if (minTitle > 1) {
      return contents;
    }
    if ((minTitleCount == 1) && (titles.size() > 1)) {
      return fixUsingAutomaticReplacement(analysis);
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
        tmp.append(TitleBuilder
            .from(title.getLevel() + 1, title.getTitle())
            .withAfter(title.getAfterTitle()).toString());
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
