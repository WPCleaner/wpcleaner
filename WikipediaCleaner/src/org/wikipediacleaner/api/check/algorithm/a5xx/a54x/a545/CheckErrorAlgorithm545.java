/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a545;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 545 of check wikipedia project.
 * Error 545: Template with deprecated parameter.
 */
public class CheckErrorAlgorithm545 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm545() {
    super("Template with deprecated parameter");
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

    // Preliminary check
    if (configByTemplateName.isEmpty()) {
      return false;
    }
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }

    // Analyze each template
    boolean result = false;
    for (PageElementTemplate template : templates) {
      result |= analyzeTemplate(analysis, errors, template);
    }
    return result;
  }

  /**
   * Analyze a template to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template to analyze.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplate(
      @Nonnull PageAnalysis analysis,
      @Nullable Collection<CheckErrorResult> errors,
      @Nonnull PageElementTemplate template) {

    // Retrieve configuration
    TemplateConfiguration templateConfig = configByTemplateName.get(template.getTemplateName());
    if (templateConfig == null) {
      return false;
    }

    // Analyze each parameter
    boolean result = false;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
      Optional<List<TemplateParameterSuggestion>> suggestions = templateConfig.analyzeParam(analysis, template, paramNum);
      if (suggestions.isPresent()) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Report error
        PageElementTemplate.Parameter templateParam = template.getParameter(paramNum);
        int beginIndex = templateParam.getBeginIndex();
        int endIndex = templateParam.getEndIndex();
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
        ParameterConfiguration paramConfig = templateConfig.getParamConfiguration(templateParam.getComputedName());
        if (paramConfig != null) {
          String explanation = paramConfig.getComment();
          if (!StringUtils.isEmpty(explanation)) {
            errorResult.addText(explanation);
          }
        }
        for (TemplateParameterSuggestion suggestion : suggestions.get()) {
          errorResult.addReplacement(suggestion.getReplacement(), suggestion.isAutomatic());
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return ((categories != null) && (!categories.isEmpty()));
  }

  /**
   * @return Tracking categories.
   */
  private List<String> getTrackingCategories() {
    return categories;
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
    List<String> categoriesName = getTrackingCategories();
    if ((categoriesName == null) || (categoriesName.isEmpty())) {
      return null;
    }
    List<Page> result = new ArrayList<>();
    API api = APIFactory.getAPI();
    for (String categoryName : categoriesName) {
      String title = wiki.getWikiConfiguration().getPageTitle(Namespace.CATEGORY, categoryName);
      Page category = DataManager.createSimplePage(wiki, title, null, null, Namespace.CATEGORY);
      try {
        api.retrieveCategoryMembers(wiki, category, 0, false, limit);
        List<Page> tmp = category.getRelatedPages(RelatedPages.CATEGORY_MEMBERS);
        if (tmp != null) {
          result.addAll(tmp);
        }
      } catch (APIException e) {
        //
      }
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

  /** Categories listing pages for this error */
  private static final String PARAMETER_CATEGORIES = "categories";

  /** Template groups */
  private static final String PARAMETER_TEMPLATE_GROUPS = "template_groups";

  /** Deprecated parameters for templates */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Replacements for parameters */
  private static final String REPLACE_PARAMETERS = "replace_parameters";

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
    configByTemplateName.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addObsoleteParameters(tmpList, configByTemplateName, group);
    }
    tmp = getSpecificProperty(REPLACE_PARAMETERS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addReplaceParameters(tmpList, configByTemplateName, group);
    }

    tmp = getSpecificProperty(PARAMETER_CATEGORIES, true, true, false);
    categories.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        categories.addAll(tmpList);
      }
    }
  }

  /** Categories listing pages for this error */
  private final List<String> categories = new ArrayList<>();

  /** Configuration for each template */
  private final Map<String, TemplateConfiguration> configByTemplateName = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_CATEGORIES,
        GT._T("Categories listing pages using templates with deprecated parameters"),
        new AlgorithmParameterElement(
            "category name",
            GT._T("Name of a category listing pages using templates with deprecated parameters")),
        true));
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
        GT._T("Templates with deprecated parameters"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of a template with deprecated parameter")),
            new AlgorithmParameterElement(
                "parameter name",
                GT._T("Name a of a deprecated parameter")),
            new AlgorithmParameterElement(
                "explanation",
                GT._T("Textual explanation about the deprecated parameter"),
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        REPLACE_PARAMETERS,
        GT._T("Replacements of deprecated parameters"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of a template with deprecated parameter")),
            new AlgorithmParameterElement(
                "parameter name",
                GT._T("Name a of a deprecated parameter")),
            new AlgorithmParameterElement(
                "replacement",
                GT._T("Name of a replacement parameter"),
                true)
        },
        true));
  }
}
