/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a569;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 569 of check wikipedia project.
 * Error 569: non-breaking space in template argument name
 */
public class CheckErrorAlgorithm569 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm569() {
    super("Non-breaking space in template argument name");
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

    return analyzeTemplates(analysis, errors);
  }

  /**
   * Analyze a page to check if errors are present in templates.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplates(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    boolean result = false;
    List<PageElementTemplate> templates = analysis.getTemplates();
    for (PageElementTemplate template : templates) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        result |= analyzeTemplateParam(analysis, errors, template, paramNum);
      }
    }
    return result;
  }

  /**
   * Analyze a template parameter to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template to check.
   * @param param Parameter to check.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplateParam(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      int paramIndex) {
    PageElementTemplate.Parameter param = template.getParameter(paramIndex);
    if ((param == null) || (param.getName() == null)) {
      return false;
    }
    String paramName = param.getName();
    if (paramName.indexOf(HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getValue()) < 0) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    final int beginIndex = param.getBeginIndex();
    final int endIndex = param.getEndIndex();
    final int equalIndex = param.getValueStartIndex();
    String contents = analysis.getContents();
    String newParamName = paramName.replace(HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getValue(), ' ').trim();
    int otherIndex = template.getParameterIndex(newParamName);
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    if (otherIndex >= 0) {
      if (StringUtils.isEmpty(param.getStrippedValue())) {
        errorResult.addReplacement("", true);
      } else {
        PageElementTemplate.Parameter otherParam = template.getParameter(otherIndex);
        if (Objects.equals(param.getStrippedValue(), otherParam.getStrippedValue())) {
          errorResult.addReplacement("", true);
        }
      }
    } else {
      String replacement =
          contents.substring(beginIndex, equalIndex).replace(HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getValue(), ' ') +
          contents.substring(equalIndex, endIndex);
      errorResult.addReplacement(replacement, true);
    }
    errors.add(errorResult);
    return true;
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
}
