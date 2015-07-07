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
import org.wikipediacleaner.api.data.PageElementFunction;


/**
 * Algorithm for analyzing error 89 of check wikipedia project.
 * Error 89: DEFAULTSORT with no space after the comma
 */
public class CheckErrorAlgorithm089 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm089() {
    super("DEFAULTSORT with no space after the comma");
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

    // Analyze each default sort
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts == null) || (defaultSorts.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementFunction defaultSort : defaultSorts) {
      if (defaultSort.getParameterCount() > 0) {
        String value = defaultSort.getParameterValue(0);
        if ((value != null) && (value.trim().length() > 0)) {
          StringBuilder replacement = new StringBuilder();
          value = value.trim();
          for (int index = 0; index < value.length(); index++) {
            char character = value.charAt(index);
            replacement.append(character);
            if ((character == ',') &&
                (index + 1 < value.length()) &&
                (Character.isLetter(value.charAt(index + 1)))) {
              replacement.append(' ');
            }
          }
          if (!value.equals(replacement.toString())) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                defaultSort.getBeginIndex(), defaultSort.getEndIndex());
            errorResult.addReplacement(PageElementFunction.createFunction(
                defaultSort.getFunctionName(),
                replacement.toString()));
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
