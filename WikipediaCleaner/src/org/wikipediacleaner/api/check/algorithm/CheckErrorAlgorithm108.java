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
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 108 of check wikipedia project.
 * Error 108: ISSN wrong checksum
 */
public class CheckErrorAlgorithm108 extends CheckErrorAlgorithmISSN {

  public CheckErrorAlgorithm108() {
    super("ISSN wrong checksum");
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
      String number = issn.getISSN();
      if ((number != null) && (number.length() == 8)) {
        char check = Character.toUpperCase(number.charAt(7));
        char computedCheck = Character.toUpperCase(
            PageElementISSN.computeChecksum(number));

        String message = null;
        if ((check != computedCheck) &&
            (Character.isDigit(computedCheck) || (computedCheck == 'X'))) {
          message = GT._(
              "The checksum is {0} instead of {1}",
              new Object[] { check, computedCheck } );
        }

        if (message != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, issn, true);
          errorResult.addPossibleAction(message, new NullActionProvider());
          addHelpNeededTemplates(analysis, errorResult, issn);
          addHelpNeededComment(analysis, errorResult, issn);
          String value = issn.getISSN();
          addSearchEngines(analysis, errorResult, value);
          if (computedCheck != check) {
            value = value.substring(0, value.length() - 1) + computedCheck;
            addSearchEngines(analysis, errorResult, value);
          }
          if (issn.isTemplateParameter()) {
            PageElementTemplate template = analysis.isInTemplate(issn.getBeginIndex());
            addSearchEngines(analysis, errorResult, template);
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
    String number = issn.getISSN();
    if (number == null) {
      return null;
    }
    char check = Character.toUpperCase(number.charAt(7));
    char computedCheck = Character.toUpperCase(PageElementISSN.computeChecksum(number));
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
    return parameters;
  }
}
