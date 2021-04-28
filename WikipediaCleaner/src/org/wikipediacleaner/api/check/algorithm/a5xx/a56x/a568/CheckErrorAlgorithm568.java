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
import java.util.Map.Entry;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a567.Numeric;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
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
    for (Entry<String, Map<String, Boolean>> templateName : templateParams.entrySet()) {
      List<PageElementTemplate> templates = analysis.getTemplates(templateName.getKey());
      for (PageElementTemplate template : templates) {
        for (Entry<String,Boolean> paramConfig : templateName.getValue().entrySet()) {
          result |= analyzeTemplateParam(analysis, errors, template, paramConfig.getKey(), paramConfig.getValue());
        }
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
   * @param paramName Name of the parameter to check.
   * @param onlyInteger True if only integer values are accepted.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplateParam(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      String paramName,
      Boolean onlyInteger) {
    int paramIndex = template.getParameterIndex(paramName);
    if (paramIndex < 0) {
      return false;
    }
    PageElementTemplate.Parameter param = template.getParameter(paramIndex);
    if (Numeric.isValidFormatnum(analysis, param.getValue(), param.getValueStartIndex())) {
      return false;
    }
    int beginIndex = param.getBeginIndex();
    int endIndex = param.getEndIndex();
    if ((analysis.getSurroundingTag(WikiTagType.NOWIKI, beginIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.TEMPLATEDATA, beginIndex) != null)) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    new NumericTemplateParam(analysis, param, onlyInteger).addSuggestions(errorResult);
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

    tmp = getSpecificProperty(PARAMETER_TEMPLATE_PARAMS, true, true, false);
    templateParams.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] tmpElement : tmpList) {
          if (tmpElement.length > 2) {
            String templateName = tmpElement[0];
            Boolean onlyInteger = Boolean.valueOf(tmpElement[1]);
            Map<String, Boolean> params = templateParams.computeIfAbsent(templateName, name -> new HashMap<>());
            for (int elementNum = 2; elementNum < tmpElement.length; elementNum++) {
              params.put(tmpElement[elementNum], onlyInteger);
            }
          }
        }
      }
    }
  }

  /** Categories for templates that can be used for an abbreviation */
  private final List<String> categories = new ArrayList<>();

  /** Template parameters that are used directly in a formatnum */
  private final Map<String, Map<String, Boolean>> templateParams = new HashMap<>();

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
  }
}
