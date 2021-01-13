/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a060;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 60 of check wikipedia project.
 * Error 60: Template parameter with problem
 */
public class CheckErrorAlgorithm060 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm060() {
    super("Template parameter with problem");
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

    // Analyze each template
    boolean result = false;
    for (PageElementTemplate template : analysis.getTemplates()) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        result |= analyzeTemplateParameter(analysis, errors, template, paramNum);
      }
    }

    return result;
  }

  /**
   * Analyze a template parameter to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template to be analyzed.
   * @param paramNum Parameter to be analyzed.
   * @return True if the error was found in the template.
   */
  private boolean analyzeTemplateParameter(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      int paramNum) {

    // Analyze parameter
    String paramValue = template.getParameterValue(paramNum);
    if (paramValue != null) {
      int squareBracketsCount = 0;
      int paramValueStartIndex = template.getParameterValueStartIndex(paramNum);
      for (int currentPos = 0; currentPos < paramValue.length(); currentPos++) {
        switch (paramValue.charAt(currentPos)) {
        case '<':
          int tmpIndex = paramValueStartIndex + currentPos;
          ContentsComment comment = analysis.comments().getAt(tmpIndex);
          if (comment != null) {
            currentPos = comment.getEndIndex() - 1 - paramValueStartIndex;
          } else {
            PageElementTag tag = analysis.isInTag(tmpIndex);
            if ((tag != null) &&
                (tag.getBeginIndex() == tmpIndex) &&
                ((WikiTagType.MATH.equals(tag.getType())) ||
                 (WikiTagType.MATH_CHEM.equals(tag.getType())) ||
                 (WikiTagType.NOWIKI.equals(tag.getType())) ||
                 (WikiTagType.SCORE.equals(tag.getType())) ||
                 (WikiTagType.SOURCE.equals(tag.getType())) ||
                 (WikiTagType.SYNTAXHIGHLIGHT.equals(tag.getType())))) {
              currentPos = tag.getCompleteEndIndex() - 1 - paramValueStartIndex;
            }
          }
          break;
        case '[':
          squareBracketsCount++;
          break;
        case ']':
          if (squareBracketsCount > 0) {
            squareBracketsCount--;
          } else {

            // Check if parameter should be ignored
            Set<String> ignoreParameters = ignoreTemplates.get(template.getTemplateName());
            if (ignoreParameters != null) {
              if (ignoreParameters.isEmpty() ||
                  ignoreParameters.contains(template.getParameterName(paramNum))) {
                return false;
              }
            }

            if (errors == null) {
              return true;
            }
            int currentIndex = currentPos;
            while ((currentIndex < paramValue.length()) &&
                   (paramValue.charAt(currentIndex) == ']')) {
              currentIndex++;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                paramValueStartIndex + currentPos,
                paramValueStartIndex + currentIndex);
            errorResult.addReplacement("");
            errors.add(errorResult);
            return true;
          }
        }
      }
    }

    return false;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of templates to ignore */
  private static final String PARAMETER_IGNORE_TEMPLATES = "ignore_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_TEMPLATES, true, true, true);
    ignoreTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String tmpElement[] : tmpList) {
          String templateName = Page.normalizeTitle(tmpElement[0]);
          Set<String> parameters = ignoreTemplates.get(templateName);
          if (parameters == null) {
            parameters = new HashSet<>();
            ignoreTemplates.put(templateName, parameters);
          }
          for (int elementNum = 1; elementNum < tmpElement.length; elementNum++) {
            parameters.add(tmpElement[elementNum]);
          }
        }
      }
    }
  }

  /** Templates to ignore */
  private final Map<String, Set<String>> ignoreTemplates = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_TEMPLATES,
        GT._T("Templates to be ignored"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "template name",
              GT._T("Name of the template")),
          new AlgorithmParameterElement(
              "parameter name",
              GT._T("Name of the parameter"),
              true, true)
        },
        true));
  }
}
