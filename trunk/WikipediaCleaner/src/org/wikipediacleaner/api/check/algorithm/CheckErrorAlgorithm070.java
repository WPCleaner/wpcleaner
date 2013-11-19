/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.NullActionProvider;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 70 of check wikipedia project.
 * Error 70: ISBN wrong length
 */
public class CheckErrorAlgorithm070 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm070() {
    super("ISBN wrong length");
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

    // Configuration
    String reasonTemplate = getSpecificProperty("reason", true, true, false);

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      String isbnNumber = isbn.getISBN();
      if (isbnNumber != null) {
        int length = isbnNumber.length();
        if ((length != 10) && (length != 13)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
          errorResult.addPossibleAction(
              GT._(
                  "The ISBN''s length is {0} instead of 10 or 13",
                  Integer.toString(length) ),
              new NullActionProvider());
          String reason = null;
          if (reasonTemplate != null) {
            reason = GT._(reasonTemplate, Integer.toString(length));
          }
          addHelpNeededTemplates(analysis, errorResult, isbn, reason);
          addHelpNeededComment(analysis, errorResult, isbn, reason);
          if (isbn.isTemplateParameter()) {
            PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
            addSearchEngines(analysis, errorResult, template);
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "reason", GT._("An explanation of the problem"));
    return parameters;
  }
}
