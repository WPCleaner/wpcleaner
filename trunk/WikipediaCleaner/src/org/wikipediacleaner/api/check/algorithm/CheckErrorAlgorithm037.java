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
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 37 of check wikipedia project.
 * Error 37: Title with special letters and no DEFAULTSORT
 */
public class CheckErrorAlgorithm037 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm037() {
    super("Title with special letters and no DEFAULTSORT");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Retrieve configuration
    int limit = 5;
    String firstCharaters = getSpecificProperty("first_characters", true, false, false);
    if (firstCharaters != null) {
      try {
        limit = Integer.valueOf(firstCharaters);
      } catch (NumberFormatException e) {
        //
      }
    }

    // Analyzing title to find special characters
    String title = analysis.getPage().getTitle();
    EnumWikipedia wiki = analysis.getWikipedia();
    boolean characterFound = false;
    boolean replaceable = true;
    int currentPos = 0;
    while ((currentPos < title.length()) && (currentPos < limit)) {
      char character = title.charAt(currentPos);
      if (!SpecialCharacters.isAuthorized(character, wiki)) {
        characterFound = true;
        String replacement = SpecialCharacters.proposeReplacement(character, wiki);
        for (int i = 0; i < replacement.length(); i++) {
          if (!SpecialCharacters.isAuthorized(replacement.charAt(i), wiki)) {
            replaceable = false;
          }
        }
      }
      currentPos++;
    }
    if (!characterFound) {
      return false;
    }

    // Searching a DEFAULTSORT tag
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts != null) && (defaultSorts.size() > 0)) {
      return false;
    }

    // Searching for Categories without a sort key
    boolean categoriesWithoutSort = false;
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return false;
    }
    for (PageElementCategory category : categories) {
      if ((category.getSort() == null) ||
          (category.getSortNotTrimmed().length() == 0)) {
        categoriesWithoutSort = true;
      }
    }
    if (!categoriesWithoutSort) {
      return false;
    }

    // Reporting error
    if (errors == null) {
      return true;
    }
    PageElementCategory category = categories.get(0);
    int beginIndex = category.getBeginIndex();
    int endIndex = category.getEndIndex();
    String contents = analysis.getContents();
    String replacement =
        createDefaultSort(analysis) + "\n" +
        contents.substring(beginIndex, endIndex);
    boolean automatic = replaceable;
    if (automatic) {
      int index = beginIndex;
      while ((index > 0) && (contents.charAt(index - 1) == ' ')) {
        index--;
      }
      if ((index > 0) && (contents.charAt(index - 1) != '\n')) {
        automatic = false;
      }
      if (analysis.getPage().isRedirect() && !automatic) {
        automatic = true;
        replacement = "\n\n" + replacement;
      }
    }
    if (automatic) {
      int index = endIndex;
      while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
        index++;
      }
      if ((index < contents.length()) && (contents.charAt(index) != '\n')) {
        automatic = false;
      }
    }
    if (automatic) {
      List<PageElementTitle> titles = analysis.getTitles();
      if ((titles != null) && (titles.size() > 0)) {
        if (beginIndex < titles.get(titles.size() - 1).getEndIndex()) {
          automatic = false;
        }
      }
    }

    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errorResult.addReplacement(replacement, GT._("Add DEFAULTSORT"), automatic);
    errors.add(errorResult);
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
