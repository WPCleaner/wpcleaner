/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a546;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;

import javax.annotation.Nullable;


/**
 * Algorithm for analyzing error 546 of check wikipedia project.
 * Error 546: Article without categories.
 */
public class CheckErrorAlgorithm546 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm546() {
    super("Article without categories");
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

    // Only in main name space
    if ((analysis.getPage().getNamespace() == null) ||
        (analysis.getPage().getNamespace() != Namespace.MAIN)) {
      return false;
    }

    // Check if categories are present
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories != null) && (!categories.isEmpty())) {
      return false;
    }

    // Do not report redirections
    if ((analysis.getPage().getRedirects() != null) &&
        (analysis.getPage().getRedirects().isRedirect())) {
      return false;
    }

    // Retrieve configuration
    for (String[] categorizingTemplate : categorizingTemplates) {
      if (categorizingTemplate.length > 0) {
        String templateName = categorizingTemplate[0];
        List<PageElementTemplate> templates = analysis.getTemplates(templateName);
        if ((templates != null) && !templates.isEmpty()) {
          if (categorizingTemplate.length < 2) {
            return false;
          }
          String paramName = categorizingTemplate[1];
          String paramValue = (categorizingTemplate.length > 2) ? categorizingTemplate[2] : null;
          for (PageElementTemplate template : templates) {
            if (hasParameter(template, paramName, paramValue)) {
              return false;
            }
          }
        }
      }
    }

    // No categories found
    return true;
  }

  private boolean hasParameter(final PageElementTemplate template, @Nullable final String name, @Nullable final String value) {
    if (Objects.equals("*", name)) {
      for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
        final String paramValue = template.getParameterValue(paramNum);
        if (Objects.equals(paramValue, value)) {
          return true;
        }
      }
    }
    int paramIndex = template.getParameterIndex(name);
    if (paramIndex < 0) {
      return false;
    }
    if (value == null) {
      return true;
    }
    return Objects.equals(value, template.getParameterValue(paramIndex));
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Deprecated parameters for templates */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    categorizingTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        categorizingTemplates.addAll(tmpList);
      }
    }
  }

  /** Categorizing templates */
  private final List<String[]> categorizingTemplates = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("List of categorizing templates"),
        new AlgorithmParameterElement[] {
          new AlgorithmParameterElement(
              "template name",
              GT._T("Name of a template that categorizes articles")),
          new AlgorithmParameterElement(
              "parameter name",
              GT._T("Name of the parameter that triggers the categorization"),
              true),
          new AlgorithmParameterElement(
              "parameter value",
              GT._T("Value of the parameter that triggers the categorization"),
              true)
        },
        true));
  }
}
