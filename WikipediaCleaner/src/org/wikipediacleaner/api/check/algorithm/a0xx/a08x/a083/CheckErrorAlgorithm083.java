/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a083;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;


/**
 * Algorithm for analyzing error 83 of check wikipedia project.
 * Error 83: Headlines start with three "=" and later with level two
 */
public class CheckErrorAlgorithm083 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm083() {
    super("Headlines start with three \"=\" and later with level two");
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

    // Check first title
    List<PageElementTitle> titles = analysis.getTitles();
    if (titles.size() == 0) {
      return false;
    }
    PageElementTitle firstTitle = titles.get(0);
    if (firstTitle.getLevel() < 3) {
      return false;
    }

    // Check every title
    boolean result = false;
    for (int titleIndex = 1; titleIndex < titles.size(); titleIndex++) {
      PageElementTitle title = titles.get(titleIndex);
      if (title.getLevel() < firstTitle.getLevel()) {
        if (errors == null) {
          return true;
        }
        result = true;
        if (titleIndex == 1) {
          CheckErrorResult errorResult = createCheckErrorResult(analysis, firstTitle.getBeginIndex(), firstTitle.getEndIndex());
          errorResult.addReplacement(
              TitleBuilder
                  .from(title.getLevel(), firstTitle.getTitle())
                  .withAfter(firstTitle.getAfterTitle()).toString(),
              firstTitle.isCoherent() && title.isCoherent());
          errors.add(errorResult);
          break;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            title.getBeginIndex(), title.getEndIndex());
        errorResult.addEditTocAction(title);
        errors.add(errorResult);
        break;
      }
    }

    return result;
  }

  private final static Set<Integer> AUTOMATIC_NAMESPACES = Stream
      .of(Namespace.MAIN, Namespace.IMAGE)
      .collect(Collectors.toCollection(HashSet::new));

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    String defaultContents = analysis.getContents();
    Integer namespace = analysis.getPage().getNamespace();
    if ((namespace == null) || !AUTOMATIC_NAMESPACES.contains(namespace)) {
      return defaultContents;
    }

    // Check first title
    List<PageElementTitle> titles = analysis.getTitles();
    if (titles.size() < 2) {
      return defaultContents;
    }
    PageElementTitle firstTitle = titles.get(0);
    if ((firstTitle.getLevel() < 3) || !firstTitle.isCoherent()) {
      return defaultContents;
    }

    // Find first title with smaller level
    int correctTitleIndex = -1;
    int delta = 0;
    for (int titleIndex = 1; (titleIndex < titles.size()) && (correctTitleIndex < 0); titleIndex++) {
      PageElementTitle title = titles.get(titleIndex);
      if (title.getLevel() < firstTitle.getLevel()) {
        if (!title.isCoherent()) {
          return defaultContents;
        }
        correctTitleIndex = titleIndex;
        delta = firstTitle.getLevel() - title.getLevel();
      }
    }
    if (correctTitleIndex < 0) {
      return defaultContents;
    }

    // Construct result
    int lastIndex = 0;
    StringBuilder newContents = new StringBuilder();
    for (int titleIndex = 0; titleIndex < correctTitleIndex; titleIndex++) {
      PageElementTitle title = titles.get(titleIndex);
      if (lastIndex < title.getBeginIndex()) {
        newContents.append(defaultContents.substring(lastIndex, title.getBeginIndex()));
      }
      newContents.append(TitleBuilder
          .from(title.getLevel() - delta, title.getTitle())
          .withAfter(title.getAfterTitle()).toString());
      lastIndex = title.getEndIndex();
    }
    if (lastIndex < defaultContents.length()) {
      newContents.append(defaultContents.substring(lastIndex));
    }

    return newContents.toString();
  }
}
