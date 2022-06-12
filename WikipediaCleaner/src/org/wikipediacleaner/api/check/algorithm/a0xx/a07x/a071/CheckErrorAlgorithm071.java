/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a071;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISBN;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 71 of check wikipedia project.
 * Error 71: ISBN wrong position of X
 */
public class CheckErrorAlgorithm071 extends CheckErrorAlgorithmISBN {

  public CheckErrorAlgorithm071() {
    super("ISBN wrong position of X");
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
      result |= analyzeISBN(analysis, errors, isbn);
    }

    return result;
  }

  /**
   * Analyze an ISBN to check if has an error.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return isbn ISBN to be checked.
   */
  private boolean analyzeISBN(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementISBN isbn) {

    // Check if an error is detected in the ISBN
    String isbnNumber = isbn.getISBN();
    if ((isbnNumber == null) || !isbn.isValid()) {
      return false;
    }
    if (shouldIgnoreError(analysis, isbn)) {
      return false;
    }
    boolean found = false;
    for (int i = 0; i < isbnNumber.length(); i++) {
      if (Character.toUpperCase(isbnNumber.charAt(i)) == 'X') {
        if ((i != 9) || (isbnNumber.length() != 10)) {
          found = true;
        }
      }
    }
    if (!found) {
      return false;
    }

    // Exclude parameters in templates
    if (isbn.isTemplateParameter() &&
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
                found = false;
              }
            }
          }
        }
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
    addHelpNeededTemplates(analysis, errorResult, isbn);
    addHelpNeededComment(analysis, errorResult, isbn);
    if (isbn.isTemplateParameter()) {
      PageElementTemplate template = analysis.isInTemplate(isbn.getBeginIndex());
      addSearchEngines(analysis, errorResult, template);
    }
    errors.add(errorResult);

    return true;
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
    return reason;
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

  /** Reason of the error */
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
