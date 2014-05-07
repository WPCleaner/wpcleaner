/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.text.MessageFormat;
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
 * Algorithm for analyzing error 73 of check wikipedia project.
 * Error 73: ISBN wrong checksum in ISBN-13
 */
public class CheckErrorAlgorithm073 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm073() {
    super("ISBN wrong checksum in ISBN-13");
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

    // Analyze each ISBN
    boolean result = false;
    List<PageElementISBN> isbns = analysis.getISBNs();
    for (PageElementISBN isbn : isbns) {
      String number = isbn.getISBN();
      if ((number != null) && (number.length() == 13)) {
        char check = Character.toUpperCase(number.charAt(12));
        char computedCheck = Character.toUpperCase(
            PageElementISBN.computeChecksum(number));
        if ((check != computedCheck) && Character.isDigit(computedCheck)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
          errorResult.addPossibleAction(
              GT._(
                  "The checksum is {0} instead of {1}",
                  new Object[] { check, computedCheck } ),
              new NullActionProvider());
          addHelpNeededTemplates(analysis, errorResult, isbn);
          addHelpNeededComment(analysis, errorResult, isbn);

          // Use search engines
          String value = number;
          addSearchEngines(analysis, errorResult, value);
          value = number.substring(0, number.length() - 1) + computedCheck;
          addSearchEngines(analysis, errorResult, value);
          if (!number.startsWith("978") && !number.startsWith("979")) {
            int count = 0;
            count += (number.charAt(0) == '9') ? 1 : 0;
            count += (number.charAt(1) == '7') ? 1 : 0;
            count += (number.charAt(2) == '8') ? 1 : 0;
            count += (number.charAt(2) == '9') ? 1 : 0;
            if (count == 2) {
              if (number.charAt(2) == '9') {
                value = "979" + number.substring(3);
              } else {
                value = "978" + number.substring(3);
              }
              if (PageElementISBN.isValid(value)) {
                addSearchEngines(analysis, errorResult, value);
              }
            }
          }
          if (number.startsWith("987")) {
            value = "978" + number.substring(3);
            if (PageElementISBN.isValid(value)) {
              addSearchEngines(analysis, errorResult, value);
            }
          }
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
   * @param isbn ISBN number.
   * @return Reason for the error.
   */
  @Override
  public String getReason(PageElementISBN isbn) {
    if (isbn == null) {
      return null;
    }
    String reasonTemplate = getSpecificProperty("reason", true, true, false);
    if (reasonTemplate == null) {
      return null;
    }
    String number = isbn.getISBN();
    if (number == null) {
      return null;
    }
    char check = Character.toUpperCase(number.charAt(12));
    char computedCheck = Character.toUpperCase(PageElementISBN.computeChecksum(number));
    return MessageFormat.format(reasonTemplate, computedCheck, check);
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
