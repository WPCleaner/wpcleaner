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
import org.wikipediacleaner.api.data.ISBNRange;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.ISBNRange.ISBNInformation;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 72 of check wikipedia project.
 * Error 72: ISBN wrong checksum in ISBN-10
 */
public class CheckErrorAlgorithm072 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm072() {
    super("ISBN wrong checksum in ISBN-10");
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
      String number = isbn.getISBN();
      if ((number != null) && (number.length() == 10)) {
        char check = Character.toUpperCase(number.charAt(9));
        char computedCheck = Character.toUpperCase(
            PageElementISBN.computeChecksum(number));

        String message = null;
        if ((check != computedCheck) &&
            (Character.isDigit(computedCheck) || (computedCheck == 'X'))) {
          message = GT._(
              "The checksum is {0} instead of {1}",
              new Object[] { check, computedCheck } );
        } else {
          ISBNInformation isbnInfo = ISBNRange.getInformation(number);
          if (isbnInfo != null) {
            if (isbnInfo.isInUnknownRange()) {
              message = GT._("There's no existing range for this ISBN");
            } else if (isbnInfo.isInReservedRange()) {
              message = GT._("This ISBN is inside a reserved range");
            }
          }
        }

        if (message != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
          errorResult.addPossibleAction(message, new NullActionProvider());
          addHelpNeededTemplates(analysis, errorResult, isbn);
          addHelpNeededComment(analysis, errorResult, isbn);
          String value = isbn.getISBN();
          addSearchEngines(analysis, errorResult, value);
          if (computedCheck != check) {
            value = value.substring(0, value.length() - 1) + computedCheck;
            addSearchEngines(analysis, errorResult, value);
          }
          value = "978" + isbn.getISBN();
          if (PageElementISBN.isValid(value)) {
            addSearchEngines(analysis, errorResult, value);
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
    String number = isbn.getISBN();
    if (number == null) {
      return null;
    }
    char check = Character.toUpperCase(number.charAt(9));
    char computedCheck = Character.toUpperCase(PageElementISBN.computeChecksum(number));
    if (check != computedCheck) {
      String reasonTemplate = getSpecificProperty("reason_checksum", true, true, false);
      if (reasonTemplate == null) {
        reasonTemplate = getSpecificProperty("reason", true, true, false);
      }
      if (reasonTemplate == null) {
        return null;
      }
      return MessageFormat.format(reasonTemplate, computedCheck, check);
    }

    // Retrieve information about ISBN number
    ISBNInformation isbnInfo = ISBNRange.getInformation(number);
    if (isbnInfo != null) {
      if (isbnInfo.isInUnknownRange()) {
        String reasonTemplate = getSpecificProperty("reason_no_range", true, true, false);
        if (reasonTemplate == null) {
          return null;
        }
        return reasonTemplate;
      }

      if (isbnInfo.isInReservedRange()) {
        String reasonTemplate = getSpecificProperty("reason_reserved", true, true, false);
        if (reasonTemplate == null) {
          return null;
        }
        return reasonTemplate;
      }
    }

    return null;
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
        "reason_checksum", GT._("An explanation of the problem (incorrect checksum)"));
    parameters.put(
        "reason_no_range", GT._("An explanation of the problem (non-existing range of ISBN numbers)"));
    parameters.put(
        "reason_reserved", GT._("An explanation of the problem (reserved range of ISBN numbers)"));
    return parameters;
  }
}
