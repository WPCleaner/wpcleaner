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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
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
      String isbnNumber = isbn.getISBN();
      if (isbnNumber != null) {

        // Analyze for error
        boolean found = false;
        for (int i = 0; i < isbnNumber.length(); i++) {
          if (Character.toUpperCase(isbnNumber.charAt(i)) == 'X') {
            if ((i != 9) || (isbnNumber.length() != 10)) {
              found = true;
            }
          }
        }

        // Exclude parameters in templates
        if (found &&
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
                    found = false;
                  }
                }
              }
            }
          }
        }

        // Report error
        if (found) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, isbn, true);
          addHelpNeededTemplates(analysis, errorResult, isbn);
          addHelpNeededComment(analysis, errorResult, isbn);
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
    return reasonTemplate;
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
