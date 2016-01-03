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
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 107 of check wikipedia project.
 * Error 107: ISSN wrong length
 */
public class CheckErrorAlgorithm107 extends CheckErrorAlgorithmISSN {

  public CheckErrorAlgorithm107() {
    super("ISSN wrong length");
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

    // Analyze each ISSN
    boolean result = false;
    List<PageElementISSN> issns = analysis.getISSNs();
    for (PageElementISSN issn : issns) {
      String issnNumber = issn.getISSN();
      if ((issnNumber != null) && (issn.isValid())) {
        int length = issnNumber.length();
        if ((length != 8) && (length != 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, issn, true);
          errorResult.addPossibleAction(
              GT._(
                  "Length of ISSN is {0} instead of 8",
                  Integer.toString(length) ),
              new NullActionProvider());
          addSuggestions(analysis, errorResult, issn);
          addHelpNeededTemplates(analysis, errorResult, issn);
          addHelpNeededComment(analysis, errorResult, issn);
          String value = issn.getISSN();
          addSearchEngines(analysis, errorResult, value);
          if (issn.isTemplateParameter()) {
            PageElementTemplate template = analysis.isInTemplate(issn.getBeginIndex());
            addSearchEngines(analysis, errorResult, template);
          }
          if ((length == 10) || (length == 13)) {
            addSearchEnginesISBN(analysis, errorResult, issn.getISSN());
          } else if (length == 7) {
            char computedCheck = PageElementISBN.computeChecksum(value + '0');
            if (computedCheck > 0) {
              addSearchEngines(analysis, errorResult, value + computedCheck);
            }
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param issn ISSN number.
   * @return Reason for the error.
   */
  @Override
  public String getReason(PageElementISSN issn) {
    if (issn == null) {
      return null;
    }
    String reasonTemplate = getSpecificProperty("reason", true, true, false);
    if (reasonTemplate == null) {
      return null;
    }
    String number = issn.getISSN();
    if (number == null) {
      return null;
    }
    int length = number.length();
    return MessageFormat.format(reasonTemplate, Integer.toString(length));
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
