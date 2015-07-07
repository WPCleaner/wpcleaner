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
import org.wikipediacleaner.api.data.PageElementISBN;


/**
 * Algorithm for analyzing error 69 of check wikipedia project.
 * Error 69: ISBN wrong syntax
 */
public class CheckErrorAlgorithm069 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm069() {
    super("ISBN wrong syntax");
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

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      if (!isbn.isCorrect() && isbn.isValid()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, false);
        addSuggestions(analysis, errorResult, isbn);
        errors.add(errorResult);
        List<String> replacements = isbn.getCorrectISBN();
        if (replacements != null) {
          for (String replacement : replacements) {
            if (!replacement.equals(analysis.getContents().substring(isbn.getBeginIndex(), isbn.getEndIndex()))) {
              errorResult.addReplacement(replacement);
            }
          }
        }
      }
    }

    // Analyze each template parameter
    /*List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        if ("ISBN10".equalsIgnoreCase(template.getParameterName(paramNum))) {
          if (errors == null) {
            return true;
          }
          result = true;
          int begin = template.getParameterNameOffset(paramNum);
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, begin,
              begin + template.getParameterName(paramNum).length());
          errors.add(errorResult);
        }
      }
    }*/

    return result;
  }

  /**
   * @param isbn ISBN number.
   * @return Reason for the error.
   */
  @Override
  public String getReason(PageElementISBN isbn) {
    return null;
  }
}
