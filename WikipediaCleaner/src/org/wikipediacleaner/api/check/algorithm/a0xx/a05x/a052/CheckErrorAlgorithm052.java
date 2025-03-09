/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a05x.a052;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 52 of check wikipedia project.
 * Error 52: Category before last headline.
 */
public class CheckErrorAlgorithm052 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm052() {
    super("Category before last headline");
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

    // Searching for last headline
    PageElementTitle title = getLastTitle(analysis);
    if (title == null) {
      return false;
    }

    // Checking every category
    boolean result = false;
    for (PageElementCategory category : analysis.getCategories()) {

      // Decide if error should be reported
      boolean shouldReport = category.getBeginIndex() < title.getBeginIndex();
      if (shouldReport && !ignoreTemplates.isEmpty()) {
        int beginIndex = category.getBeginIndex();
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if (template != null) {
          Set<String> parameters = ignoreTemplates.get(template.getTemplateName());
          if (parameters != null) {
            if (parameters.isEmpty()) {
              shouldReport = false;
            } else {
              PageElementTemplate.Parameter param = template.getParameterAtIndex(beginIndex);
              if ((param != null) && parameters.contains(param.getComputedName())) {
                shouldReport = false;
              }
            }
          }
        }
      }
      if (shouldReport && (analysis.getSurroundingTag(WikiTagType.NOWIKI, category.getBeginIndex()) != null)) {
        shouldReport = false;
      }

      // Report error
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            category.getBeginIndex(),
            category.getEndIndex());
        String categoryName = category.getName();
        if ((categoryName == null) || categoryName.isEmpty()) {
          errorResult.addReplacement("", true);
        }
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Retrieve last title.
   * 
   * @param analysis Page analysis.
   * @return Last title.
   */
  private PageElementTitle getLastTitle(PageAnalysis analysis) {
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || titles.isEmpty()) {
      return null;
    }
    PageElementTitle title = null;
    for (PageElementTitle tmpTitle : titles) {
      if ((title == null) || (title.getBeginIndex() < tmpTitle.getBeginIndex())) {
        title = tmpTitle;
      }
    }
    return title;
  }

  /**
   * Retrieve last default sort.
   * 
   * @param analysis Page analysis.
   * @return Last default sort.
   */
  private PageElementFunction getLastDefaultSort(PageAnalysis analysis) {
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts == null) || defaultSorts.isEmpty()) {
      return null;
    }
    return defaultSorts.get(defaultSorts.size() - 1);
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    String contents = analysis.getContents();
    Integer namespace = analysis.getPage().getNamespace();
    if (!Objects.equals(namespace, Namespace.MAIN) &&
        !Objects.equals(namespace, Namespace.CATEGORY) &&
        !Objects.equals(namespace, Namespace.MEDIA)) {
      return contents;
    }

    // Check if errors are present
    PageElementTitle title = getLastTitle(analysis);
    if (title == null) {
      return contents;
    }
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || categories.isEmpty()) {
      return contents;
    }
    if (categories.get(0).getBeginIndex() > title.getBeginIndex()) {
      return contents;
    }

    // Decide where the categories can be moved
    int position = contents.length();
    PageElementFunction defaultSort = getLastDefaultSort(analysis);
    if ((defaultSort != null) &&
        (defaultSort.getParameterCount() > 1)) {
      defaultSort = null;
    }
    if ((defaultSort != null) &&
        (defaultSort.getParameterCount() == 1)) {
      if (defaultSort.getParameterFullText(0).contains("}") ||
          defaultSort.getParameterFullText(0).contains("}")) {
        defaultSort = null;
      }
    }
    if ((defaultSort != null) &&
        (defaultSort.getBeginIndex() > title.getBeginIndex())) {
      position = defaultSort.getEndIndex();
      defaultSort = null;
    } else {
      PageElementCategory categoryOk = null;
      for (PageElementCategory tmpCategory : categories) {
        if ((categoryOk == null) &&
            (tmpCategory.getBeginIndex() > title.getBeginIndex())) {
          categoryOk = tmpCategory;
        }
      }
      if (categoryOk != null) {
        position = categoryOk.getBeginIndex();
      } else {
        PageElementLanguageLink languageOk = null;
        List<PageElementLanguageLink> languages = analysis.getLanguageLinks();
        if (languages != null) {
          for (PageElementLanguageLink tmpLanguage : languages) {
            if ((languageOk == null) &&
                (tmpLanguage.getBeginIndex() > title.getBeginIndex())) {
              languageOk = tmpLanguage;
            }
          }
        }
        if (languageOk != null) {
          position = languageOk.getBeginIndex();
        }
      }
    }

    // Build the result
    StringBuilder newCategories = new StringBuilder();
    StringBuilder newContent = new StringBuilder();
    int lastIndex = 0;
    for (PageElementCategory category : categories) {

      // Only move categories on a separate line
      boolean shouldMove = false;
      int categoryBeginIndex = category.getBeginIndex();
      int categoryEndIndex = category.getEndIndex();
      if (categoryBeginIndex < title.getBeginIndex()) {
        shouldMove = true;
        if ((categoryBeginIndex > 0) &&
            (contents.charAt(categoryBeginIndex - 1) != '\n')) {
          shouldMove = false;
        }
        if ((categoryEndIndex < contents.length()) &&
            (contents.charAt(categoryEndIndex) != '\n')) {
          shouldMove = false;
        }
      }

      if (shouldMove) {
        if ((analysis.getSurroundingTag(WikiTagType.INCLUDEONLY, categoryBeginIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.NOINCLUDE, categoryBeginIndex) != null) ||
            (analysis.getSurroundingTag(WikiTagType.ONLYINCLUDE, categoryBeginIndex) != null)) {
          shouldMove = false;
        }
      }

      if (shouldMove) {

        // Move default sort
        if ((defaultSort != null) &&
            (defaultSort.getBeginIndex() < categoryBeginIndex)) {
          int defaultSortBeginIndex = defaultSort.getBeginIndex();
          int defaultSortEndIndex = defaultSort.getEndIndex();
          if (defaultSortBeginIndex > lastIndex) {
            newContent.append(contents, lastIndex, defaultSortBeginIndex);
          }
          lastIndex = defaultSortEndIndex;
          if (contents.charAt(lastIndex) == '\n') {
            if ((defaultSortBeginIndex <= 0) || (contents.charAt(defaultSortBeginIndex - 1) == '\n')) {
              lastIndex++;
            }
          }
          if (!newCategories.isEmpty()) {
            newCategories.append('\n');
          }
          newCategories.append(contents, defaultSortBeginIndex, defaultSortEndIndex);
          defaultSort = null;
        }

        // Move category
        if (categoryBeginIndex > lastIndex) {
          newContent.append(contents, lastIndex, categoryBeginIndex);
        }
        int tmpIndex = categoryEndIndex;
        while ((tmpIndex < contents.length()) &&
            (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        if ((tmpIndex < contents.length()) &&
            (contents.charAt(tmpIndex) == '<')) {
          ContentsComment comment = analysis.comments().getBeginsAt(tmpIndex);
          if (comment != null) {
            categoryEndIndex = comment.getEndIndex();
          }
        }
        lastIndex = categoryEndIndex;
        if (contents.charAt(lastIndex) == '\n') {
          if ((categoryBeginIndex <= 0) ||
              (contents.charAt(categoryBeginIndex - 1) == '\n')) {
            lastIndex++;
          }
        }
        if (!newCategories.isEmpty()) {
          newCategories.append('\n');
        }
        newCategories.append(contents, categoryBeginIndex, categoryEndIndex);
      }
    }
    if (newCategories.isEmpty()) {
      return fixUsingAutomaticReplacement(analysis);
    }
    if (lastIndex < position) {
      newContent.append(contents, lastIndex, position);
    }
    if (newContent.charAt(newContent.length() - 1) != '\n') {
      newContent.append('\n');
    }
    newContent.append(newCategories);
    if (position < contents.length()) {
      if (newContent.charAt(newContent.length() - 1) != '\n') {
        newContent.append('\n');
      }
      newContent.append(contents.substring(position));
    }

    return newContent.toString();
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
