/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;


/**
 * Algorithm for analyzing error 524 of check wikipedia project.
 * Error 524: Duplicated template argument
 */
public class CheckErrorAlgorithm524 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm524() {
    super("Duplicated template argument");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each template
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || templates.isEmpty()) {
      return false;
    }
    HashMap<String, Parameter> names = new HashMap<String, PageElementTemplate.Parameter>();
    boolean result = false;
    for (PageElementTemplate template : templates) {
      int nbParam = template.getParameterCount();
      if (nbParam > 1) {
        names.clear();
        for (int numParam = 0; numParam < nbParam; numParam++) {
          Parameter param = template.getParameter(numParam);
          String paramName = param.getComputedName();
          Parameter existingParam = names.get(paramName);
          if (existingParam == null) {
            names.put(paramName, param);
          } else {
            if (errors == null) {
              return true;
            }
            result = true;
            int paramBegin = param.getPipeIndex();
            int paramEnd = template.getEndIndex() - 2;
            if (numParam + 1 < nbParam) {
              Parameter nextParam = template.getParameter(numParam + 1);
              paramEnd = nextParam.getPipeIndex();
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, paramBegin, paramEnd);
            String existingValue = existingParam.getValue();
            String value = param.getValue();
            if ((existingValue != null) && (existingValue.equals(value))) {
              errorResult.addReplacement("", true);
            }
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
