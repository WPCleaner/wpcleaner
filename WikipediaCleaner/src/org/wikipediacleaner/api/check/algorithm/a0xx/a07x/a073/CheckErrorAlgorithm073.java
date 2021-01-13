/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a073;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISBN;
import org.wikipediacleaner.api.data.ISBNRange;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.ISBNRange.ISBNInformation;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
      if ((number != null) && (number.length() == 13) && isbn.isValid()) {
        char check = Character.toUpperCase(number.charAt(12));
        char computedCheck = Character.toUpperCase(
            PageElementISBN.computeChecksum(number));

        String message = null;
        if ((check != computedCheck) && Character.isDigit(computedCheck)) {
          message = GT._T(
              "The checksum is {0} instead of {1}",
              new Object[] { check, computedCheck } );
        } else {
          ISBNInformation isbnInfo = ISBNRange.getInformation(number);
          if (isbnInfo != null) {
            if (isbnInfo.isInUnknownRange()) {
              message = GT._T("There's no existing range for this ISBN");
            } else if (isbnInfo.isInReservedRange()) {
              message = GT._T("This ISBN is inside a reserved range");
            }
          }
        }

        if (message != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
          errorResult.addText(message);
          addHelpNeededTemplates(analysis, errorResult, isbn);
          addHelpNeededComment(analysis, errorResult, isbn);

          // Add original ISBN
          addSearchEngines(analysis, errorResult, number);

          // Add search engines using other parameters of the template
          if (isbn.isTemplateParameter()) {
            PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
            addSearchEngines(analysis, errorResult, template);
          }

          // Add search for other identifiers
          errorResult.addPossibleAction(new SimpleAction(GT._T(
              "Search as OCLC"),
              new ActionExternalViewer(MessageFormat.format("http://worldcat.org/oclc/{0}", number))));
          errorResult.addPossibleAction(new SimpleAction(GT._T(
              "Search as LCCN"),
              new ActionExternalViewer(MessageFormat.format("http://lccn.loc.gov/{0}", number))));

          // Add ISSN if number starts with 977=Prefix for ISSN
          if (number.startsWith("977")) { // Prefix for ISSN
            String value = number.substring(3, 10);
            char checkISSN = PageElementISSN.computeChecksum(value + '0');
            if (checkISSN > 0) {
              addSearchEnginesISSN(analysis, errorResult, value + checkISSN);
            }
          }

          // Add ISBN with modified checksum
          List<String> searchISBN = new ArrayList<>();
          if (computedCheck != check) {
            String value = number.substring(0, number.length() - 1) + computedCheck;
            addSearchISBN(searchISBN, value, false);
          }

          // Try specific replacements if ISBN doesn't start with 978 or 979
          if (!number.startsWith("978") && !number.startsWith("979")) {
            int count = 0;
            count += (number.charAt(0) == '9') ? 1 : 0;
            count += (number.charAt(1) == '7') ? 1 : 0;
            count += (number.charAt(2) == '8') ? 1 : 0;
            count += (number.charAt(2) == '9') ? 1 : 0;
            if (count == 2) {
              String value = ((number.charAt(2) == '9') ? "979" : "978") + number.substring(3);
              addSearchISBN(searchISBN, value, false);
            }
          }

          // Try ISBN-10
          if (number.startsWith("978")) {
            String value = number.substring(3);
            addSearchISBN(searchISBN, value, false);
          }

          // Add ISBN with characters inversion
          if (number.length() == 13) {
            int previousChar = -1;
            for (int currentChar = 0; currentChar < number.length(); currentChar++) {
              if (Character.isDigit(number.charAt(currentChar))) {
                if (previousChar >= 0) {
                  String value =
                      number.substring(0, previousChar) +
                      number.charAt(currentChar) +
                      number.substring(previousChar + 1, currentChar) +
                      number.charAt(previousChar) +
                      number.substring(currentChar + 1);
                  addSearchISBN(searchISBN, value, false);
                }
                previousChar = currentChar;
              }
            }
          }

          // Add ISBN with one modified digit
          if (number.length() == 13) {
            for (int currentChar = 0; currentChar < number.length(); currentChar++) {
              if (Character.isDigit(number.charAt(currentChar))) {
                for (char newChar = '0'; newChar <= '9'; newChar++) {
                  String value =
                      number.substring(0, currentChar) +
                      newChar +
                      number.substring(currentChar + 1);
                  addSearchISBN(searchISBN, value, false);
                }
              }
            }
          }

          // Add direct search engines
          addSearchEngines(
              analysis, errorResult, searchISBN,
              GT._T("Similar ISBN"));

          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param searchISBN List of ISBN.
   * @param isbn ISBN to be added.
   * @param force True if ISBN should be added even if incorrect.
   */
  private void addSearchISBN(List<String> searchISBN, String isbn, boolean force) {
    if (!searchISBN.contains(isbn)) {
      if (force ||
          (PageElementISBN.computeChecksum(isbn) == isbn.charAt(isbn.length() - 1))) {
        searchISBN.add(isbn);
      }
    }
    if ((isbn.length() == 13) && isbn.startsWith("978")) {
      isbn = isbn.substring(3);
      if (!searchISBN.contains(isbn)) {
        if (PageElementISBN.computeChecksum(isbn) == isbn.charAt(isbn.length() - 1)) {
          searchISBN.add(isbn);
        }
      }
    }
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
    char check = Character.toUpperCase(number.charAt(12));
    char computedCheck = Character.toUpperCase(PageElementISBN.computeChecksum(number));

    // Invalid checksum
    if ((check != computedCheck) && Character.isDigit(computedCheck)) {
      if (reasonChecksum == null) {
        return null;
      }
      return MessageFormat.format(reasonChecksum, computedCheck, check);
    }

    // Retrieve information about ISBN number
    ISBNInformation isbnInfo = ISBNRange.getInformation(number);
    if (isbnInfo != null) {
      if (isbnInfo.isInUnknownRange()) {
        return reasonNoRange;
      }

      if (isbnInfo.isInReservedRange()) {
        return reasonReserved;
      }
    }

    return null;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Reason of the error */
  private static final String PARAMETER_REASON = "reason";

  /** Reason of the error: checksum */
  private static final String PARAMETER_REASON_CHECKSUM = "reason_checksum";

  /** Reason of the error: no range */
  private static final String PARAMETER_REASON_NO_RANGE = "reason_no_range";

  /** Reason of the error: reserved range */
  private static final String PARAMETER_REASON_RESERVED = "reason_reserved";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    reasonChecksum = getSpecificProperty(PARAMETER_REASON_CHECKSUM, true, true, false);
    if (reasonChecksum == null) {
      reasonChecksum = getSpecificProperty(PARAMETER_REASON, true, true, false);
    }
    reasonNoRange = getSpecificProperty(PARAMETER_REASON_NO_RANGE, true, true, false);
    reasonReserved = getSpecificProperty(PARAMETER_REASON_RESERVED, true, true, false);
  }

  /** Reason of the error: checksum */
  private String reasonChecksum = null;

  /** Reason of the error: no range */
  private String reasonNoRange = null;

  /** Reason of the error: reserved range */
  private String reasonReserved = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_REASON_CHECKSUM,
        GT._T("An explanation of the problem (incorrect checksum)"),
        new AlgorithmParameterElement(
            "text",
            GT._T("An explanation of the problem"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_REASON_NO_RANGE,
        GT._T("An explanation of the problem (non-existing range of ISBN numbers)"),
        new AlgorithmParameterElement(
            "text",
            GT._T("An explanation of the problem"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_REASON_RESERVED,
        GT._T("An explanation of the problem (reserved range of ISBN numbers)"),
        new AlgorithmParameterElement(
            "text",
            GT._T("An explanation of the problem"))));
  }
}
