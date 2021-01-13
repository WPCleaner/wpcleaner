/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a070;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISBN;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
      String isbnNumber = isbn.getISBN();
      if ((isbnNumber != null) && (isbn.isValid())) {

        // Analyze for error
        int length = isbnNumber.length();
        boolean isError = false;
        if ((length != 10) && (length != 13) && (length != 0)) {
          isError = true;
        }

        // Exclude parameters in templates
        if (isError &&
            isbn.isTemplateParameter() &&
            analysis.isInNamespace(Namespace.TEMPLATE)) {
          PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(isbn.getBeginIndex());
            if (param != null) {
              List<PageElementFunction> functions = analysis.getFunctions();
              if (functions != null) {
                for (PageElementFunction function : functions) {
                  int functionIndex = function.getBeginIndex();
                  if ((template == analysis.isInTemplate(functionIndex)) &&
                      (param == template.getParameterAtIndex(functionIndex))) {
                    isError = false;
                  }
                }
              }
            }
          }
        }

        // Report error
        if (isError) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
          errorResult.addText(
              GT._T(
                  "Length of ISBN is {0} instead of 10 or 13",
                  Integer.toString(length) ));
          addSuggestions(analysis, errorResult, isbn);
          addHelpNeededTemplates(analysis, errorResult, isbn);
          addHelpNeededComment(analysis, errorResult, isbn);

          // Add original ISBN
          addSearchEngines(analysis, errorResult, isbnNumber);

          // Add search engines using other parameters of the template
          if (isbn.isTemplateParameter()) {
            PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
            addSearchEngines(analysis, errorResult, template);
          }

          // Add search for other identifiers
          errorResult.addPossibleAction(new SimpleAction(GT._T(
              "Search as OCLC"),
              new ActionExternalViewer(MessageFormat.format("http://worldcat.org/oclc/{0}", isbnNumber))));
          errorResult.addPossibleAction(new SimpleAction(GT._T(
              "Search as LCCN"),
              new ActionExternalViewer(MessageFormat.format("http://lccn.loc.gov/{0}", isbnNumber))));

          // Add search for potential ISSN
          if (length == 8) {
            addSearchEnginesISSN(analysis, errorResult, isbnNumber);
          }

          // Add ISBN with added checksum
          List<String> searchISBN = new ArrayList<>();
          if ((length == 9) || (length == 12)) {
            char computedCheck = PageElementISBN.computeChecksum(isbnNumber + '0');
            if (computedCheck > 0) {
              addSearchISBN(searchISBN, isbnNumber + computedCheck, false);
            }
          }

          // Try specific replacements
          if ((length == 12) && isbnNumber.startsWith("78")) {
            addSearchISBN(searchISBN, "9" + isbnNumber, false);
          }

          // Add ISBN with one extra digit
          if ((length == 9) || (length == 12)) {
            for (int currentChar = 0; currentChar < isbnNumber.length(); currentChar++) {
              if (Character.isDigit(isbnNumber.charAt(currentChar))) {
                for (char newChar = '0'; newChar <= '9'; newChar++) {
                  String value =
                      isbnNumber.substring(0, currentChar) +
                      newChar +
                      isbnNumber.substring(currentChar);
                  addSearchISBN(searchISBN, value, false);
                }
              }
            }
          }

          // Add ISBN with one digit removed
          if ((length == 11) || (length == 14)) {
            for (int currentChar = 0; currentChar < isbnNumber.length(); currentChar++) {
              char currentValue = isbnNumber.charAt(currentChar);
              if (Character.isDigit(currentValue) || (currentValue == 'X')) {
                String value =
                    isbnNumber.substring(0, currentChar) +
                    isbnNumber.substring(currentChar + 1);
                addSearchISBN(searchISBN, value, false);
              }
            }
          }

          // Add ISBN with consecutive digits removed
          if (isbnNumber.startsWith("978") || isbnNumber.startsWith("979")) {
            if (length > 14) {
              for (int currentChar = 0; currentChar < 13; currentChar++) {
                String value =
                    isbnNumber.substring(0, currentChar) +
                    isbnNumber.substring(currentChar + length - 13);
                addSearchISBN(searchISBN, value, false);
              }
            }
          } else {
            if (length > 11) {
              for (int currentChar = 0; currentChar < 10; currentChar++) {
                String value =
                    isbnNumber.substring(0, currentChar) +
                    isbnNumber.substring(currentChar + length - 10);
                addSearchISBN(searchISBN, value, false);
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
    boolean checksumOk = (PageElementISBN.computeChecksum(isbn) == isbn.charAt(isbn.length() - 1)); 
    if (!searchISBN.contains(isbn)) {
      if (force || checksumOk) {
        searchISBN.add(isbn);
      }
    }

    // Suggestions for ISBN-10 based on ISBN-13s
    if ((isbn.length() == 13) && (isbn.startsWith("978"))) {
      String isbn10 = isbn.substring(3);
      char checksum = PageElementISBN.computeChecksum(isbn10);
      if (!searchISBN.contains(isbn10) && (checksum == isbn10.charAt(isbn10.length() - 1))) {
        searchISBN.add(isbn10);
      }

      if (checksumOk && (checksum > 0)) {
        isbn10 = isbn10.substring(0, isbn10.length() - 1) + checksum;
        if (!searchISBN.contains(isbn10)) {
          searchISBN.add(isbn10);
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
    if ((isbn == null) || (reason == null)) {
      return null;
    }
    String number = isbn.getISBN();
    if (number == null) {
      return null;
    }
    int length = number.length();
    return MessageFormat.format(reason, Integer.toString(length));
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Reason of the error */
  private static final String PARAMETER_REASON = "reason";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    reason = getSpecificProperty(PARAMETER_REASON, true, true, false);
  }

  /** Links to ignore */
  private String reason = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_REASON,
        GT._T("An explanation of the problem"),
        new AlgorithmParameterElement(
            "text",
            GT._T("An explanation of the problem"))));
  }
}
