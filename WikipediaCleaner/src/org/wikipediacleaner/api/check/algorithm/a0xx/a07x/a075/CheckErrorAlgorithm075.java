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

    // Start with bullet or number list items
    PageElementListItem initialItem = listItems.get(initialIndex);
    String initialIndicators = initialItem.getIndicators();
    if (!initialIndicators.startsWith("*") && !initialIndicators.startsWith("#")) {
      return false;
    }
    if (analysis.comments().isAt(initialItem.getBeginIndex())) {
      return false;
    }

    // Check following list items
    int lastIndex = initialItem.getEndIndex();
    int currentIndex = initialIndex + 1;
    boolean result = false;
    String contents = analysis.getContents();
    boolean emptyLine = false;
    while (currentIndex < listItems.size()) {

      // Check that following list item is close to the previous one
      while ((lastIndex < contents.length()) && (contents.charAt(lastIndex) == '\n')) {
        lastIndex++;
      }
      emptyLine |= lastIndex > listItems.get(currentIndex - 1).getEndIndex() + 1; 
      PageElementListItem currentItem = listItems.get(currentIndex);
      if (currentItem.getBeginIndex() > lastIndex) {
        return result;
      }

      // Check what indicators are used
      String currentIndicators = currentItem.getIndicators();
      int incorrectIndicators = 0;
      while ((incorrectIndicators < currentIndicators.length()) &&
             (currentIndicators.charAt(incorrectIndicators) == ':')) {
        incorrectIndicators++;
      }
      if ((incorrectIndicators < 1) || (incorrectIndicators >= currentIndicators.length())) {
        return result;
      }
      if (analysis.comments().isAt(currentItem.getBeginIndex())) {
        return result;
      }

      // Report error
      if ("*#".indexOf(currentIndicators.charAt(incorrectIndicators)) >= 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        int beginIndex = currentItem.getBeginIndex();
        int endIndex = currentItem.getEndIndex();
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
        boolean automatic =
            !emptyLine &&
            (incorrectIndicators <= initialIndicators.length());
        int maxLengthRetrieved = Math.min(initialIndicators.length(), incorrectIndicators);
        String replacement =
            initialIndicators.substring(0, maxLengthRetrieved) +
            contents.substring(beginIndex + maxLengthRetrieved, endIndex);
        String text =
            initialIndicators.substring(0, maxLengthRetrieved) +
            currentIndicators.substring(maxLengthRetrieved) +
            " ...";
        errorResult.addReplacement(replacement, text, automatic);
        errors.add(errorResult);
      } else {
        return result;
      }
      lastIndex = currentItem.getEndIndex();
      currentIndex++;
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
