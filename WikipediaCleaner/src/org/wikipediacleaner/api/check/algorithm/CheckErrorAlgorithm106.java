/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.Replacement;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 106 of check wikipedia project.
 * Error 106: ISSN wrong syntax
 */
public class CheckErrorAlgorithm106 extends CheckErrorAlgorithmISSN {

  public CheckErrorAlgorithm106() {
    super("ISSN wrong syntax");
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
      boolean isError = false;
      if (!issn.isCorrect() && issn.isValid()) {
        isError = true;
      }

      // Exclude special configured values for ISSN
      if (isError &&
          issn.isTemplateParameter()) {
        WPCConfiguration config = analysis.getWPCConfiguration();
        List<String[]> specialValues = config.getStringArrayList(
            WPCConfigurationStringList.ISSN_SPECIAL_VALUES);
        if ((specialValues != null) && !specialValues.isEmpty()) {
          PageElementTemplate template = analysis.isInTemplate(issn.getBeginIndex());
          if (template != null) {
            Parameter param = template.getParameterAtIndex(issn.getBeginIndex());
            if ((param != null) &&
                (param.getName() != null) &&
                (param.getName().trim().length() > 0)) {
              String name = param.getName().trim();
              for (String[] specialValue : specialValues) {
                if ((specialValue.length > 2) &&
                    (Page.areSameTitle(template.getTemplateName(), specialValue[0])) &&
                    (name.equals(specialValue[1])) &&
                    (issn.getISSNNotTrimmed().equals(specialValue[2]))) {
                  isError = false;
                }
              }
            }
          }
        }
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
        CheckErrorResult errorResult = createCheckErrorResult(analysis, issn, false);
        addSuggestions(analysis, errorResult, issn);
        errors.add(errorResult);
        List<Replacement> replacements = issn.getCorrectISSN();
        if (replacements != null) {
          for (Replacement replacement : replacements) {
            String newText = replacement.replacement;
            if ((newText != null) &&
                !newText.equals(analysis.getContents().substring(issn.getBeginIndex(), issn.getEndIndex()))) {
              errorResult.addReplacement(replacement);
            }
          }
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
    return reasonTemplate;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
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
