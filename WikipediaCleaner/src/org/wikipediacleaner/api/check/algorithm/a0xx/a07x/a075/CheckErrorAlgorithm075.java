/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a075;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 75 of check wikipedia project.
 * Error 75: Indented list
 */
public class CheckErrorAlgorithm075 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm075() {
    super("Indented list");
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

    // Analyze each list item
    List<PageElementListItem> listItems = analysis.getListItems();
    if (listItems == null) {
      return false;
    }
    boolean result = false;
    for (int index = 0; index < listItems.size(); index++) {
      result |= analyzeListItem(analysis, errors, listItems, index);
    }
    return result;
  }

  /**
   * Analyze a list item to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param listItems List items
   * @param initialIndex Index of the initial item to analyze.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeListItem(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      List<PageElementListItem> listItems,
      int initialIndex) {
    PageElementListItem initialItem = listItems.get(initialIndex);
    String initialIndicators = initialItem.getIndicators();
    if (!initialIndicators.startsWith("*") && !initialIndicators.startsWith("#")) {
      return false;
    }

    int currentIndex = initialIndex + 1;
    boolean result = false;
    while (currentIndex < listItems.size()) {
      PageElementListItem currentItem = listItems.get(currentIndex);
      String currentIndicators = currentItem.getIndicators();
      if (currentIndicators.startsWith(":*") || currentIndicators.startsWith(":#")) {
        if (errors == null) {
          return true;
        }
        result = true;
        String contents = analysis.getContents();
        int beginIndex = currentItem.getBeginIndex();
        int endIndex = currentItem.getEndIndex();
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
        boolean automatic =
            (initialIndicators.length() == 1) &&
            (currentIndicators.charAt(0) == initialIndicators.charAt(0));
        String replacement =
            initialIndicators.substring(0, 1) +
            contents.substring(beginIndex + 1, endIndex);
        String text =
            initialIndicators.substring(0, 1) +
            currentIndicators.substring(1) +
            " ...";
        errorResult.addReplacement(replacement, text, automatic);
        errors.add(errorResult);
      } else {
        return result;
      }
      currentIndex++;
    }

    return result;
  }
}
