/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
        boolean isError = false;
        int length = issnNumber.length();
        if ((length != 8) && (length != 0)) {
          isError = true;
        }

        // Exclude parameters in templates
        if (isError &&
            issn.isTemplateParameter() &&
            analysis.isInNamespace(Namespace.TEMPLATE)) {
          PageElementTemplate template = analysis.isInTemplate(issn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(issn.getBeginIndex());
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
          CheckErrorResult errorResult = createCheckErrorResult(analysis, issn, true);
          errorResult.addText(
              GT._T(
                  "Length of ISSN is {0} instead of 8",
                  Integer.toString(length) ));
          addSuggestions(analysis, errorResult, issn);
          addHelpNeededTemplates(analysis, errorResult, issn);
          addHelpNeededComment(analysis, errorResult, issn);

          // Add original ISSN
          String originalValue = issn.getISSN();
          addSearchEngines(analysis, errorResult, originalValue);

          // Add search engines using other parameters of the template
          if (issn.isTemplateParameter()) {
            PageElementTemplate template = analysis.isInTemplate(issn.getBeginIndex());
            addSearchEngines(analysis, errorResult, template);
          }

          // Add search for potential ISBN
          if ((length == 10) || (length == 13)) {
            addSearchEnginesISBN(analysis, errorResult, issn.getISSN());
          }

          // Add search for other identifiers
          errorResult.addPossibleAction(new SimpleAction(GT._T(
              "Search as OCLC"),
              new ActionExternalViewer(MessageFormat.format("http://worldcat.org/oclc/{0}", originalValue))));
          errorResult.addPossibleAction(new SimpleAction(GT._T(
              "Search as LCCN"),
              new ActionExternalViewer(MessageFormat.format("http://lccn.loc.gov/{0}", originalValue))));

          // Add ISSN with added checksum
          List<String> searchISSN = new ArrayList<>();
          if (length == 7) {
            char computedCheck = PageElementISSN.computeChecksum(originalValue + '0');
            if (computedCheck > 0) {
              addSearchISSN(searchISSN, originalValue + computedCheck, false);
            }
          }

          // Add ISSN for EAN 977
          if ((originalValue.length() == 13) && (originalValue.startsWith("977"))) {
            String value = originalValue.substring(3, 10);
            char computedCheck = PageElementISSN.computeChecksum(value + '0');
            if (computedCheck > 0) {
              addSearchISSN(searchISSN, value + computedCheck, false);
            }
          }
          // Add ISSN with one extra digit
          if (originalValue.length() == 7) {
            for (int currentChar = 0; currentChar < originalValue.length(); currentChar++) {
              if (Character.isDigit(originalValue.charAt(currentChar))) {
                for (char newChar = '0'; newChar <= '9'; newChar++) {
                  String value =
                      originalValue.substring(0, currentChar) +
                      newChar +
                      originalValue.substring(currentChar);
                  addSearchISSN(searchISSN, value, false);
                }
              }
            }
          }

          // Add ISSN with one digit removed
          if (originalValue.length() == 9) {
            for (int currentChar = 0; currentChar < originalValue.length(); currentChar++) {
              if (Character.isDigit(originalValue.charAt(currentChar))) {
                String value =
                    originalValue.substring(0, currentChar) +
                    originalValue.substring(currentChar + 1);
                addSearchISSN(searchISSN, value, false);
              }
            }
          }

          // Add direct search engines
          addSearchEngines(
              analysis, errorResult, searchISSN,
              GT._T("Similar ISSN"));

          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param searchISSN List of ISSN.
   * @param issn ISSN to be added.
   * @param force True if ISSN should be added even if incorrect.
   */
  private void addSearchISSN(List<String> searchISSN, String issn, boolean force) {
    if (!searchISSN.contains(issn)) {
      if (force ||
          (PageElementISSN.computeChecksum(issn) == issn.charAt(issn.length() - 1))) {
        searchISSN.add(issn);
      }
    }
  }

  /**
   * @param issn ISSN number.
   * @return Reason for the error.
   */
  @Override
  public String getReason(PageElementISSN issn) {
    if ((issn == null) || (reasonTemplate == null)) {
      return null;
    }
    String number = issn.getISSN();
    if (number == null) {
      return null;
    }
    int length = number.length();
    return MessageFormat.format(reasonTemplate, Integer.toString(length));
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Explanation of the problem */
  private static final String PARAMETER_REASON = "reason";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    reasonTemplate = getSpecificProperty(PARAMETER_REASON, true, true, false);
  }

  /** Explanation of the problem */
  private String reasonTemplate = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_REASON,
        GT._T("An explanation of the problem")));
  }
}
