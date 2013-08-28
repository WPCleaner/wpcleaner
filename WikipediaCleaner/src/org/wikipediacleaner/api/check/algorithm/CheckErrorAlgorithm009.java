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
import org.wikipediacleaner.api.data.PageElementCategory;


/**
 * Algorithm for analyzing error 09 of check wikipedia project.
 * Error 09: Categories more at one line
 */
public class CheckErrorAlgorithm009 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm009() {
    super("Categories more at one line");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Check every category
    List<PageElementCategory> categories = pageAnalysis.getCategories();
    if (categories.size() < 2) {
      return false;
    }
    int maxCategory = categories.size();
    boolean result = false;
    int currentCategory = 0;
    String contents = pageAnalysis.getContents();
    while (currentCategory < maxCategory) {

      // Group categories in the same line
      boolean endFound = false;
      int lastCategory = currentCategory;
      while ((!endFound) && (lastCategory < maxCategory - 1)) {
        int maxIndex = categories.get(lastCategory + 1).getBeginIndex();
        int currentIndex = categories.get(lastCategory).getEndIndex();
        while ((!endFound) && (currentIndex < maxIndex)) {
          if (contents.charAt(currentIndex) == '\n') {
            endFound = true;
          }
          currentIndex++;
        }
        if (!endFound) {
          lastCategory++;
        }
      }

      // Register error
      if (lastCategory > currentCategory) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check first category in the line
        StringBuilder replacement = new StringBuilder();
        int beginIndex = categories.get(currentCategory).getBeginIndex();
        if ((currentCategory == 0) && (pageAnalysis.getPage().isRedirect())) {
          int crIndex = contents.indexOf('\n');
          if ((crIndex < 0) || (crIndex > beginIndex)) {
            replacement.append("\n\n");
            while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == ' ')) {
              beginIndex--;
            }
          }
        }

        // Put each category on a different line
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            beginIndex,
            categories.get(lastCategory).getEndIndex());
        boolean automatic = true;
        for (int i = currentCategory; i < lastCategory; i++) {
          int end = categories.get(i + 1).getBeginIndex();
          for (int index = categories.get(i).getEndIndex(); index < end; index++) {
            char currentChar = contents.charAt(index);
            if ((currentChar != ' ') && (currentChar != '\n')) {
              automatic = false;
            }
          }
          replacement.append(contents.substring(
              categories.get(i).getBeginIndex(),
              end).trim());
          replacement.append('\n');
        }
        String replacementText = (lastCategory - currentCategory > 1) ?
            "[[...]]\u21B5...\u21B5[[...]]" : "[[...]]\u21B5[[...]]";
        replacement.append(contents.substring(
            categories.get(lastCategory).getBeginIndex(),
            categories.get(lastCategory).getEndIndex()));
        errorResult.addReplacement(replacement.toString(), replacementText, automatic);
        errors.add(errorResult);
      }
      currentCategory = lastCategory + 1;
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
  public String automaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
