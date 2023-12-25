/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a572;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 572 of check wikipedia project.
 * Error 572: Unknown values for parameters
 */
public class CheckErrorAlgorithm572 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm572() {
    super("Unknown values");
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
   * Analyze a template to see if it has unknown values for parameters.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template to analyze.
   * @return True if the template has unknown values for parameters.
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
   * Analyze a template parameter to see if it has an unknown value.
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
      errorResult.addReplacement(suggestion.getReplacement(), automatic);
    }
    List<PageElementTemplate.Parameter> paramsWithSameValue = IntStream.range(0, template.getParameterCount())
        .filter(paramIndex -> paramIndex != paramNum)
        .mapToObj(paramIndex -> template.getParameter(paramIndex))
        .filter(param -> Objects.equals(param.getValue(), templateParam.getValue()))
        .collect(Collectors.toList());
    if (!paramsWithSameValue.isEmpty()) {
      paramsWithSameValue.forEach(param -> errorResult.addText(GT._T("Same value as parameter {0}", param.getName())));
      boolean automatic = paramsWithSameValue.stream()
          .anyMatch(param -> templateConfiguration.isPossibleConfusion(templateParam.getName(), param.getName()));
      errorResult.addReplacement("", automatic);
    }
    for (String otherParamName : templateConfiguration.getPossibleConfusion(templateParam.getName())) {
      if (template.getParameterIndex(otherParamName) < 0) {
        errorResult.addReplacement(TemplateParameterSuggestion
            .replaceParam(analysis.getContents(), templateParam, otherParamName, templateParam.getValueNotTrimmed(), false)
            .getReplacement());
      }
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (dumpAnalysis != null);
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    List<Page> result = new ArrayList<>();

    // Use internal links
    if (dumpAnalysis != null) {
      API api = APIFactory.getAPI();
      Page page = DataManager.createSimplePage(wiki, dumpAnalysis, null, null, null);
      try {
        api.retrieveLinks(wiki, page, null, null, false, false);
        if (page.getLinks() != null) {
          result.addAll(page.getLinks());
        }
      } catch (APIException e) {
        //
      }
    }

    Collections.sort(result);

    // Limit result size
    while (result.size() > limit) {
      result.remove(result.size() - 1);
    }

    return result;
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

  /** Page containing a dump analysis of the error */
  private static final String PARAMETER_DUMP_ANALYSIS = "dump_analysis";

  /** Template groups */
  private static final String PARAMETER_TEMPLATE_GROUPS = "template_groups";

  /** Templates and parameters that are checked */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Values that can be safely replaced */
  private static final String PARAMETER_REPLACE_VALUES = "replace_values";

  /** Possible confusions in parameter names */
  private static final String PARAMETER_CONFUSION = "confusion";
  
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
    List<String[]> generalList = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.TEMPLATE_GROUPS);
    if (generalList != null) {
      group.addGroups(generalList);
    }

    tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    configurationByTemplateName.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addKnownParameters(tmpList, configurationByTemplateName, group);
    }
    tmp = getSpecificProperty(PARAMETER_REPLACE_VALUES, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addValuesToReplace(tmpList, configurationByTemplateName, group);
    }
    tmp = getSpecificProperty(PARAMETER_CONFUSION, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addConfusions(tmpList, configurationByTemplateName, group);
    }

    dumpAnalysis = getSpecificProperty(PARAMETER_DUMP_ANALYSIS, true, true, false);
  }

  /** Page containing a dump analysis */
  private String dumpAnalysis = null;

  /** Templates and parameters that are checked */
  private final Map<String, TemplateConfiguration> configurationByTemplateName = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_DUMP_ANALYSIS,
        GT._T("A page containing a dump analysis for this error."),
        new AlgorithmParameterElement(
            "page name",
            GT._T("A page containing a dump analysis for this error."))));
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
        GT._T("Templates for which unknown values should be verified"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter")),
            new AlgorithmParameterElement(
                "case sensitive",
                GT._T("True if the value is case sensitive")),
            new AlgorithmParameterElement(
                "value",
                GT._T("Value of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_REPLACE_VALUES,
        GT._T("Template values which can be safely replaced"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter")),
            new AlgorithmParameterElement(
                "initial value",
                GT._T("Initial value of the parameter")),
            new AlgorithmParameterElement(
                "target value",
                GT._T("Target value of the parameter"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_CONFUSION,
        GT._T("Possible confusions with another parameter"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter")),
            new AlgorithmParameterElement(
                "value",
                GT._T("Value of the parameter"),
                false,
                true)
        },
        true));
  }
}
