/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 92 of check wikipedia project.
 * Error 92: Headline double
 */
public class CheckErrorAlgorithm092 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm092() {
    super("Headline double");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    boolean result = false;
    int previousTitleLevel = 0;
    HashMap<Integer, HashMap<String, PageElementTitle>> titles = new HashMap<>();
    for (PageElementTitle title : analysis.getTitles()) {

      // Clean up titles with a lower level
      int titleLevel = title.getLevel();
      if (titleLevel < previousTitleLevel) {
        for (int i = previousTitleLevel; i > titleLevel; i--) {
          titles.remove(Integer.valueOf(i));
        }
      }

      // Analyze current level
      if (titleLevel <= maxLevel) {
        HashMap<String, PageElementTitle> knownTitles = titles.get(Integer.valueOf(titleLevel));
        String titleValue = title.getTitle();
        if (knownTitles == null) {
          knownTitles = new HashMap<>();
          knownTitles.put(titleValue, title);
          titles.put(Integer.valueOf(titleLevel), knownTitles);
        } else if (!knownTitles.containsKey(titleValue)) {
          if (onlyConsecutive) {
            knownTitles.clear();
          }
          knownTitles.put(titleValue, title);
        } else {
          if (errors == null) {
            return true;
          }
          result = true;
          PageElementTitle previousTitle = knownTitles.get(titleValue);
          if (previousTitle != null) {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                previousTitle.getBeginIndex(), previousTitle.getEndIndex(),
                ErrorLevel.CORRECT);
            errors.add(errorResult);
            knownTitles.put(titleValue, null);
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              title.getBeginIndex(), title.getEndIndex());
          errorResult.addEditTocAction(title);
          errors.add(errorResult);
        }
      }

      previousTitleLevel = titleLevel;
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
    String contents = analysis.getContents();
    if ((!analysis.getPage().isArticle()) ||
        (!analysis.getWPCConfiguration().isEncyclopedicNamespace(analysis.getPage().getNamespace())) ||
        (!analysis.areTitlesReliable())) {
      return contents;
    }
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles == null) || (titles.size() < 2)) {
      return contents;
    }

    // Fix double headlines
    int lastIndex = 0;
    StringBuilder buffer = new StringBuilder();
    for (int i = 1; i < titles.size(); i++) {
      PageElementTitle previousTitle = titles.get(i - 1);
      PageElementTitle currentTitle = titles.get(i);
      if ((previousTitle.getLevel() <= maxLevel) &&
          (previousTitle.getLevel() == currentTitle.getLevel()) &&
          (previousTitle.getTitle().equals(currentTitle.getTitle()))) {

        // Analyze if first title can be removed
        String betweenTitles = contents.substring(
            previousTitle.getEndIndex(), currentTitle.getBeginIndex()).trim();
        boolean shouldRemove = false;
        if (betweenTitles.length() == 0) {
          shouldRemove = true;
        } else {
          int tmpIndex = currentTitle.getEndIndex();
          while ((tmpIndex < contents.length()) &&
                 (Character.isWhitespace(contents.charAt(tmpIndex)))) {
            tmpIndex++;
          }
          if ((tmpIndex < contents.length()) &&
              (contents.startsWith(betweenTitles, tmpIndex))) {
            shouldRemove = true;
          }
        }
        if (shouldRemove) {
          if (previousTitle.getBeginIndex() > lastIndex) {
            buffer.append(contents.substring(lastIndex, previousTitle.getBeginIndex()));
            lastIndex = previousTitle.getBeginIndex();
          }
          lastIndex = currentTitle.getBeginIndex();
        } else {

          // Analyze if second title can be removed
          PageElementTitle nextTitle = (i + 1 < titles.size()) ? titles.get(i + 1) : null;
          String afterTitle = contents.substring(
              currentTitle.getEndIndex(),
              (nextTitle != null) ? nextTitle.getBeginIndex() : contents.length()).trim();
          if (afterTitle.length() == 0) {
            shouldRemove = true;
          } else {
            int tmpIndex = previousTitle.getEndIndex();
            while ((tmpIndex < contents.length()) &&
                   (Character.isWhitespace(contents.charAt(tmpIndex)))) {
              tmpIndex++;
            }
            if ((tmpIndex < contents.length()) &&
                (contents.startsWith(afterTitle, tmpIndex))) {
              shouldRemove = true;
            }
          }
          if (shouldRemove) {
            if (currentTitle.getBeginIndex() > lastIndex) {
              buffer.append(contents.substring(lastIndex, currentTitle.getBeginIndex()));
              lastIndex = currentTitle.getBeginIndex();
            }
            lastIndex = (nextTitle != null) ? nextTitle.getBeginIndex() : contents.length();
          }
        }
      }
    }
    if (lastIndex == 0) {
      return contents;
    }
    if (lastIndex < contents.length()) {
      buffer.append(contents.substring(lastIndex));
    }
    return buffer.toString();
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Parameter to limit the level of titles */
  private static final String PARAMETER_MAX_LEVEL = "max_level";

  /** Parameter to report only consecutive titles */
  private static final String PARAMETER_ONLY_CONSECUTIVE = "only_consecutive";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_MAX_LEVEL, true, true, false);
    maxLevel = Integer.MAX_VALUE;
    if (tmp != null) {
      try {
        maxLevel = Integer.parseInt(tmp, 10);
      } catch (NumberFormatException e) {
        //
      }
    }

    tmp = getSpecificProperty(PARAMETER_ONLY_CONSECUTIVE, true, false, false);
    onlyConsecutive = (tmp != null) ? Boolean.valueOf(tmp) : false;
  }

  /** Limit the level of titles */
  private int maxLevel = Integer.MAX_VALUE;

  /** True to report only consecutive titles */
  private boolean onlyConsecutive = false;

  /**
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        PARAMETER_MAX_LEVEL,
        GT._T("Maximum level of titles to report"));
    parameters.put(
        PARAMETER_ONLY_CONSECUTIVE,
        GT._T("To report only consecutive titles"));
    return parameters;
  }
}
