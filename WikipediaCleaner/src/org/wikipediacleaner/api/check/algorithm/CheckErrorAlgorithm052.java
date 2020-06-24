/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsComment;


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
      if (category.getBeginIndex() < title.getBeginIndex()) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis,
            category.getBeginIndex(),
            category.getEndIndex());
        String categoryName = category.getName();
        if ((categoryName == null) || ("".equals(categoryName))) {
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
    if (!analysis.getPage().isArticle()) {
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

        // Move default sort
        if ((defaultSort != null) &&
            (defaultSort.getBeginIndex() < categoryBeginIndex)) {
          int defaultSortBeginIndex = defaultSort.getBeginIndex();
          int defaultSortEndIndex = defaultSort.getEndIndex();
          if (defaultSortBeginIndex > lastIndex) {
            newContent.append(contents.substring(lastIndex, defaultSortBeginIndex));
          }
          lastIndex = defaultSortEndIndex;
          if (contents.charAt(lastIndex) == '\n') {
            if ((defaultSortBeginIndex <= 0) || (contents.charAt(defaultSortBeginIndex - 1) == '\n')) {
              lastIndex++;
            }
          }
          if (newCategories.length() > 0) {
            newCategories.append('\n');
          }
          newCategories.append(contents.substring(defaultSortBeginIndex, defaultSortEndIndex));
          defaultSort = null;
        }

        // Move category
        if (categoryBeginIndex > lastIndex) {
          newContent.append(contents.substring(lastIndex, categoryBeginIndex));
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
        if (newCategories.length() > 0) {
          newCategories.append('\n');
        }
        newCategories.append(contents.substring(categoryBeginIndex, categoryEndIndex));
      }
    }
    if (newCategories.length() == 0) {
      return fixUsingAutomaticReplacement(analysis);
    }
    if (lastIndex < position) {
      newContent.append(contents.substring(lastIndex, position));
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
}
