/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a568;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateParameterSuggestion;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 568 of check wikipedia project.
 * Error 568: non-numeric formatnum arguments
 */
public class CheckErrorAlgorithm568 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm568() {
    super("Non-numeric formatnum arguments (in templates)");
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
    if (configurationByTemplateName.isEmpty()) {
      return false;
    }

    boolean result = false;
    List<PageElementTemplate> templates = analysis.getTemplates();
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
   * @param template Template to be checked.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template) {
    TemplateConfiguration templateConfiguration = configurationByTemplateName.get(template.getTemplateName());
    if (templateConfiguration == null) {
      return false;
    }
    boolean result = false;
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
      result |= analyzeTemplateParam(analysis, errors, template, paramNum, templateConfiguration);
    }
    return result;
  }

  /**
   * Analyze a template parameter to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template to check.
   * @param paramName Name of the parameter to check.
   * @param onlyInteger True if only integer values are accepted.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplateParam(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      int paramNum,
      TemplateConfiguration templateConfiguration) {

    // Check if an error is present
    Optional<List<TemplateParameterSuggestion>> suggestions = templateConfiguration.analyzeParam(analysis, template, paramNum);
    if (!suggestions.isPresent()) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Report error
    PageElementTemplate.Parameter param = template.getParameter(paramNum);
    int beginIndex = param.getBeginIndex();
    int endIndex = param.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    errorResult.addText(GT._T("Template {0}", template.getTemplateName()));
    for (TemplateParameterSuggestion suggestion : suggestions.get()) {
      boolean automatic = suggestion.isAutomatic();
      automatic &=
          (suggestion.getParamName() == null) ||
          (template.getParameterValue(suggestion.getParamName()) == null) ||
          StringUtils.equals(param.getName(), suggestion.getParamName());
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
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
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

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Categories listing pages for this error */
  private static final String PARAMETER_CATEGORIES = "categories";

  /** Template parameters that are used directly in a formatnum */
  private static final String PARAMETER_TEMPLATE_PARAMS = "template_params";

  /** Template parameters that can store references */
  private static final String PARAMETER_REF_PARAMS = "ref_params";

  /** Suffixes that can be removed */
  private static final String PARAMETER_REMOVE_SUFFIX = "remove_suffix";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_CATEGORIES, true, true, false);
    categories.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        categories.addAll(tmpList);
      }
    }

    configurationByTemplateName.clear();
    tmp = getSpecificProperty(PARAMETER_TEMPLATE_PARAMS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addTemplateParams(tmpList, configurationByTemplateName);
    }
    tmp = getSpecificProperty(PARAMETER_REF_PARAMS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addRefParams(tmpList, configurationByTemplateName);
    }
    tmp = getSpecificProperty(PARAMETER_REMOVE_SUFFIX, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addRemoveSuffixes(tmpList, configurationByTemplateName);
    }
  }

  /** Categories for templates that can be used for an abbreviation */
  private final List<String> categories = new ArrayList<>();

  /** Configuration */
  private final Map<String, TemplateConfiguration>  configurationByTemplateName = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_CATEGORIES,
        GT._T("Categories listing pages using non-numeric {0} arguments", "{{formatnum:}}"),
        new AlgorithmParameterElement(
            "category name",
            GT._T("Name of a category listing pages using non-numeric {0} arguments", "{{formatnum:}}")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATE_PARAMS,
        GT._T("Template parameters that should only contain numeric arguments"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement("template name", GT._T("Template name")),
            new AlgorithmParameterElement("only integer", GT._T("If parameters accept only integer values")),
            new AlgorithmParameterElement("parameter name", GT._T("Parameter name"), false, true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_REF_PARAMS,
        GT._T("Template parameters that can contain a reference"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement("template name", GT._T("Template name")),
            new AlgorithmParameterElement("initial parameter name", GT._T("Parameter name with the formatnum")),
            new AlgorithmParameterElement("reference parameter name", GT._T("Parameter name that can contain a reference"))
        },
        true));
  }
}
