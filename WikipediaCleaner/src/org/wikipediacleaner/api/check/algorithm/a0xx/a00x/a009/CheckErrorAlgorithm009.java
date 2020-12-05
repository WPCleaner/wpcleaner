/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a00x.a009;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


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

    // Check every category
    List<PageElementCategory> categories = analysis.getCategories();
    if (categories.size() < 2) {
      return false;
    }
    int maxCategory = categories.size();
    boolean result = false;
    int currentCategory = 0;
    String contents = analysis.getContents();
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
        if (currentCategory == 0) {
          int tmpIndex = beginIndex;
          int nbCr = 0;
          while ((tmpIndex > 0) &&
                 ((contents.charAt(tmpIndex - 1) == ' ') ||
                  (contents.charAt(tmpIndex - 1) == '\n'))) {
            if (contents.charAt(tmpIndex - 1) == '\n') {
              nbCr++;
            }
            tmpIndex--;
          }
          PageElementTemplate template = analysis.isInTemplate(tmpIndex - 1);
          PageElementFunction function = analysis.isInFunction(tmpIndex - 1);
          if ((template != null) && (template.getEndIndex() == tmpIndex)) {
            replacement.append("\n\n");
            beginIndex = tmpIndex;
          } else if ((function != null) && (function.getEndIndex() == tmpIndex)) {
            if (function.getMagicWord().isPossibleAlias(MagicWord.DEFAULT_SORT)) {
              tmpIndex = function.getBeginIndex();
              while ((tmpIndex > 0) &&
                     ((contents.charAt(tmpIndex - 1) == ' ') ||
                      (contents.charAt(tmpIndex - 1) == '\n'))) {
                tmpIndex--;
              }
              replacement.append("\n\n");
              replacement.append(contents.substring(function.getBeginIndex(), function.getEndIndex()));
            }
            replacement.append("\n");
            beginIndex = tmpIndex;
          } else if (analysis.getPage().getRedirects().isRedirect() && (nbCr == 0)) {
            int crIndex = contents.indexOf('\n');
            if ((crIndex < 0) || (crIndex > tmpIndex)) {
              replacement.append("\n\n");
              beginIndex = tmpIndex;
            }
          }
        }

        // Put each category on a different line
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            beginIndex,
            categories.get(lastCategory).getEndIndex());
        boolean automatic = true;
        if ((analysis.isInTemplate(beginIndex) != null) ||
            (analysis.isInFunction(beginIndex) != null)) {
          automatic = false;
        }
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
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
