/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a564;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 564 of check wikipedia project.
 * Error 564: Unknown parameters
 */
public class CheckErrorAlgorithm564 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm564() {
    super("Unknown parameters");
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

    // Preliminary checks
    if (configurationByTemplateName.isEmpty()) {
      return false;
    }
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || templates.isEmpty()) {
      return false;
    }

    // Check each template
    boolean result = false;
    for (PageElementTemplate template : templates) {
      result |= analyzeTemplate(analysis, errors, template);
    }

    return result;
  }

  /**
   * Analyze a template to see if it has unused parameters.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template to analyze.
   * @return True if the template has unused parameters.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template) {
    if ((analysis == null) || (template == null)) {
      return false;
    }
    TemplateConfiguration templateConfiguration = configurationByTemplateName.get(template.getTemplateName());
    if (templateConfiguration == null) {
      return false;
    }
    boolean result = false;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
      result |= analyzeTemplateParameter(analysis, errors, template, paramNum, templateConfiguration);
    }
    return result;
  }

  /**
   * Analyze a template parameter to see if it is unused.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param templateParameter Parameter to analyze.
   * @param templateConfiguration Configuration for the template.
   * @return
   */
  private boolean analyzeTemplateParameter(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      int paramNum,
      TemplateConfiguration templateConfiguration) {

    // Check if there's an error
    Optional<List<TemplateParameterSuggestion>> suggestions = templateConfiguration.analyzeParam(analysis, template, paramNum);
    if (!suggestions.isPresent()) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Report error
    PageElementTemplate.Parameter templateParam = template.getParameter(paramNum);
    int beginIndex = templateParam.getBeginIndex();
    int endIndex = templateParam.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    for (TemplateParameterSuggestion suggestion : suggestions.get()) {
      boolean automatic = suggestion.isAutomatic();
      automatic &= (suggestion.getParamName() == null) || (template.getParameterValue(suggestion.getParamName()) == null);
      errorResult.addReplacement(suggestion.getReplacement(), automatic);
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
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Template groups */
  private static final String PARAMETER_TEMPLATE_GROUPS = "template_groups";

  /** Templates and parameters that are checked */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Parameters that can be safely deleted */
  private static final String PARAMETER_DELETE_PARAMETERS = "delete_parameters";

  /** Values that can be safely deleted */
  private static final String PARAMETER_DELETE_VALUES = "delete_values";

  /** Parameters that can be safely commented */
  private static final String PARAMETER_COMMENT_PARAMETERS = "comment_parameters";

  /** Parameters that can be safely replaced */
  private static final String PARAMETER_REPLACE_PARAMETERS = "replace_parameters";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATE_GROUPS, true, true, false);
    TemplateConfigurationGroup group = new TemplateConfigurationGroup();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      group.addGroups(tmpList);
    }

    tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    configurationByTemplateName.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addKnownParameters(tmpList, configurationByTemplateName, group);
    }
    tmp = getSpecificProperty(PARAMETER_DELETE_PARAMETERS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addParametersToDelete(tmpList, configurationByTemplateName, group);
    }
    tmp = getSpecificProperty(PARAMETER_DELETE_VALUES, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addValuesToDelete(tmpList, configurationByTemplateName, group);
    }
    tmp = getSpecificProperty(PARAMETER_COMMENT_PARAMETERS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addParametersToComment(tmpList, configurationByTemplateName, group);
    }
    tmp = getSpecificProperty(PARAMETER_REPLACE_PARAMETERS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addParametersToReplace(tmpList, configurationByTemplateName, group);
    }
  }

  /** Templates and parameters that are checked */
  private final Map<String, TemplateConfiguration> configurationByTemplateName = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATE_GROUPS,
        GT._T("Groups of templates"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "group",
                GT._T("Name of the group")),
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of a template in the group"),
                false,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates for which unknown parameters should be verified"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_DELETE_PARAMETERS,
        GT._T("Template parameters which can be safely deleted"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_DELETE_VALUES,
        GT._T("Template parameters which can be safely deleted if they have a given value"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "value",
                GT._T("Value of the parameter")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_COMMENT_PARAMETERS,
        GT._T("Template parameters which can be safely commented"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_DELETE_PARAMETERS,
        GT._T("Template parameters which can be safely deleted"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_REPLACE_PARAMETERS,
        GT._T("Template parameters which can be safely replaced"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "initial param",
                GT._T("Initial name of the parameter"),
                true,
                true),
            new AlgorithmParameterElement(
                "target param",
                GT._T("Target name of the parameter"),
                true,
                true)
        },
        true));
  }
}
