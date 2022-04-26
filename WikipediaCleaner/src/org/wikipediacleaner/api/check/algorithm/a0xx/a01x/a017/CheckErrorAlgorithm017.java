/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a01x.a017;

import java.util.ArrayList;
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
import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.SimpleMagicWordType;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 17 of check wikipedia project.
 * Error 17: Category duplication
 */
public class CheckErrorAlgorithm017 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Delete all"),
  };

  public CheckErrorAlgorithm017() {
    super("Category duplication");
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

    // Case sensitiveness
    final Namespace namespace = analysis.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
    final EnumCaseSensitiveness sensitive = (namespace != null) ? namespace.getCaseSensitiveness() : EnumCaseSensitiveness.UNKNOWN;

    // Group categories by name
    final List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return false;
    }
    final Map<String, List<PageElementCategory>> groupedCategories = new HashMap<>();
    categories.forEach(category -> addCategoryToGroup(analysis, groupedCategories, category, sensitive));

    // Compute index of last title
    final List<PageElementTitle> titles = analysis.getTitles();
    int lastTitleIndex = 0;
    if ((titles != null) && (!titles.isEmpty())) {
      lastTitleIndex = titles.get(titles.size() - 1).getEndIndex();
    }

    // Check each category
    boolean result = false;
    for (PageElementCategory category : categories) {
      result |= analyzeCategory(analysis, errors, category, sensitive, groupedCategories, lastTitleIndex);
    }

    return result;
  }

  /**
   * Add a category to its group.
   * 
   * @param analysis Page analysis.
   * @param groupedCategories Group of categories.
   * @param category Category.
   * @param sensitive Sensitiveness of the category name.
   */
  private void addCategoryToGroup(
      PageAnalysis analysis,
      Map<String, List<PageElementCategory>> groupedCategories,
      PageElementCategory category,
      EnumCaseSensitiveness sensitive) {
    // For redirects, do not count the target of the redirection
    if (analysis.getPage().getRedirects().isRedirect()) {
      String contents = analysis.getContents();
      int tmpIndex = ContentsUtil.moveIndexBeforeWhitespace(contents, category.getBeginIndex() - 1);
      if (tmpIndex > 0) {
        MagicWord magicWord = analysis.getWikiConfiguration().getMagicWordByType(SimpleMagicWordType.REDIRECT);
        if (magicWord.isPossibleAlias(contents.substring(0, tmpIndex + 1).trim())) {
          return;
        }
      }
    }

    String name = sensitive.normalize(category.getName());
    groupedCategories.computeIfAbsent(name, key -> new ArrayList<>()).add(category);
  }

  /**
   * Analyze a category to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param category Category to analyze.
   * @param sensitive Sensitiveness of the category name.
   * @param groupedCategories Grouped categories.
   * @param lastTitleIndex Index of the last title.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeCategory(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementCategory category,
      EnumCaseSensitiveness sensitive,
      Map<String, List<PageElementCategory>> groupedCategories,
      int lastTitleIndex) {

    // Report error only if more than one category in the group
    String name = sensitive.normalize(category.getName());
    List<PageElementCategory> groupCategory = groupedCategories.get(name);
    if ((groupCategory == null) || (groupCategory.size() <= 1)) {
      return false;
    }
    if (!ignoreTemplates.isEmpty()) {
      int beginIndex = category.getBeginIndex();
      PageElementTemplate template = analysis.isInTemplate(beginIndex);
      if (template != null) {
        Set<String> parameters = ignoreTemplates.get(template.getTemplateName());
        if (parameters != null) {
          if (parameters.isEmpty()) {
            return false;
          }
          PageElementTemplate.Parameter param = template.getParameterAtIndex(beginIndex);
          if ((param != null) && parameters.contains(param.getComputedName())) {
            return false;
          }
        }
      }
    }
    if (errors == null) {
      return true;
    }

    // Mark category kept as correct
    PageElementCategory keepCategory = keepCategory(groupCategory, lastTitleIndex);
    if (keepCategory == category) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis,
          category.getBeginIndex(),
          category.getEndIndex(),
          CheckErrorResult.ErrorLevel.CORRECT);
      errors.add(errorResult);
      return true;
    }

    // Mark other categories as incorrect
    String contents = analysis.getContents();
    int beginIndex = category.getBeginIndex();
    while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == ' ')) {
      beginIndex--;
    }
    boolean beginLine = (beginIndex == 0) || (contents.charAt(beginIndex - 1) == '\n');
    int endIndex = category.getEndIndex();
    while ((endIndex < contents.length()) && (contents.charAt(endIndex) == ' ')) {
      endIndex++;
    }
    boolean endLine = (endIndex >= contents.length()) || (contents.charAt(endIndex) == '\n');
    if (beginLine && endLine) {
      endIndex = Math.min(endIndex + 1, contents.length());
    }
    if (!beginLine) {
      beginIndex = category.getBeginIndex();
    }
    if (!endLine) {
      endIndex = category.getEndIndex();
    }

    // Decide in the fix can be automatic
    boolean automatic = false;
    if (category.getBeginIndex() > keepCategory.getBeginIndex()) {
      int currentIndex = keepCategory.getEndIndex();
      boolean finished = false;
      while (!finished && (currentIndex < category.getBeginIndex())) {
        char currentChar = contents.charAt(currentIndex);
        if ((currentChar == ' ') || (currentChar == '\n')) {
          currentIndex++;
        } else {
          PageElementCategory nextCategory = analysis.isInCategory(currentIndex);
          if (nextCategory != null) {
            currentIndex = nextCategory.getEndIndex();
          } else {
            finished = true;
          }
        }
      }
      if (currentIndex >= keepCategory.getBeginIndex()) {
        if ((category.getSort() != null) &&
            (category.getSort().length() > 0)) {
          if (category.getSort().equals(keepCategory.getSort())) {
            automatic = true;
          }
        } else {
          if ((keepCategory.getSort() == null) ||
              (keepCategory.getSort().length() == 0)) {
            automatic = true;
          }
        }
      }
    }
    if ((analysis.isInFunction(beginIndex) != null) ||
        (analysis.isInFunction(keepCategory.getBeginIndex()) != null)) {
      automatic = false;
    }

    // Mark the error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errorResult.addReplacement("", GT._T("Delete"), automatic);
    errors.add(errorResult);
    return true;
  }

  /**
   * @param categories List of categories for the same name.
   * @param lastTitle Index of the last title.
   * @return Which category should be kept.
   */
  private PageElementCategory keepCategory(
      List<PageElementCategory> categories, int lastTitle) {

    // First: category after last title and with sort key
    for (PageElementCategory category : categories) {
      if ((category.getBeginIndex() >= lastTitle) &&
          (category.getSort() != null) &&
          (category.getSort().length() > 0)) {
        return category;
      }
    }

    // Second: category after last title
    for (PageElementCategory category : categories) {
      if (category.getBeginIndex() >= lastTitle) {
        return category;
      }
    }

    // Third: category with sort key
    for (PageElementCategory category : categories) {
      if ((category.getSort() != null) &&
          (category.getSort().length() > 0)) {
        return category;
      }
    }

    // Last: last category
    return categories.get(categories.size() - 1);
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates to be ignored */
  private static final String PARAMETER_IGNORE_TEMPLATES = "ignore_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_TEMPLATES, true, true, false);
    ignoreTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] tmpElement : tmpList) {
          if (tmpElement.length > 0) {
            Set<String> parameters = ignoreTemplates.computeIfAbsent(
                Page.normalizeTitle(tmpElement[0]),
                k -> new HashSet<>());
            for (int elementNum = 1; elementNum < tmpElement.length; elementNum++) {
              parameters.add(tmpElement[elementNum]);
            }
          }
        }
      }
    }
  }

  /** Templates to be ignored */
  private final Map<String, Set<String>> ignoreTemplates = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_TEMPLATES,
        GT._T("A list of templates in which categories should be ignored."),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("A template in which categories should be ignored.")),
            new AlgorithmParameterElement(
                "parameter name",
                GT._T("A template parameter in which categories should be ignored."),
                true, true)
        },
        true));
  }
}
