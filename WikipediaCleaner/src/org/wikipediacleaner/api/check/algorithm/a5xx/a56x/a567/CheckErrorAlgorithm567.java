/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a567;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.magicword.FunctionMagicWordType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 567 of check wikipedia project.
 * Error 567: non-numeric formatnum arguments
 */
public class CheckErrorAlgorithm567 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm567() {
    super("Non-numeric formatnum arguments");
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

    return analyzeFunctions(analysis, errors);
  }

  /**
   * Analyze a page to check if errors are present in functions.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeFunctions(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementFunction> functions = analysis.getFunctions();
    if (functions.isEmpty()) {
      return false;
    }

    boolean result = false;
    for (PageElementFunction function : functions) {
      result |= analyzeFunction(analysis, errors, function);
    }
    return result;
  }

  /**
   * Analyze a function to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param function Function to check.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeFunction(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementFunction function) {
    if (function.getMagicWord().getType() != FunctionMagicWordType.FORMAT_NUM) {
      return false;
    }
    if (NumericFormatnum.isValidFormatnum(analysis, function)) {
      return false;
    }
    if (function.getParameterCount() == 0) {
      return false;
    }
    int beginIndex = function.getBeginIndex();
    int endIndex = function.getEndIndex();
    if ((analysis.getSurroundingTag(WikiTagType.NOWIKI, beginIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.TEMPLATEDATA, beginIndex) != null)) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    new NumericFormatnum(analysis, function, prefixes, suffixes).addSuggestions(errorResult);
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

  /** Prefixes that can be safely extracted */
  private static final String PARAMETER_PREFIXES = "prefixes";

  /** Suffixes that can be safely extracted */
  private static final String PARAMETER_SUFFIXES = "suffixes";

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

    tmp = getSpecificProperty(PARAMETER_PREFIXES, true, true, false);
    prefixes.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        prefixes.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_SUFFIXES, true, true, false);
    suffixes.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        suffixes.addAll(tmpList);
      }
    }
  }

  /** Categories for templates that can be used for an abbreviation */
  private final List<String> categories = new ArrayList<>();

  /** Prefixes that can be safely extracted */
  private final List<String> prefixes = new ArrayList<>();

  /** Suffixes that can be safely extracted */
  private final List<String> suffixes = new ArrayList<>();

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
        PARAMETER_PREFIXES,
        GT._T("Prefixes that can be safely extracted"),
        new AlgorithmParameterElement(
            "text",
            GT._T("Prefix that can be safely extracted")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_SUFFIXES,
        GT._T("Suffixes that can be safely extracted"),
        new AlgorithmParameterElement(
            "text",
            GT._T("Suffix that can be safely extracted")),
        true));
  }
}
