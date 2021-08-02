/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a570;

import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 570 of check wikipedia project.
 * Error 570: mw-content-ltr/rtl without dir or lang
 */
public class CheckErrorAlgorithm570 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm570() {
    super("mw-content-ltr/rtl without dir or lang");
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

    return analyzeTags(analysis, errors);
  }

  /**
   * Analyze a page to check if errors are present in tags.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    boolean result = false;
    List<PageElementTag> tags = analysis.getTags();
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, tag);
    }
    return result;
  }

  /**
   * Analyze a tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Tag to check.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag) {
    if (!tag.isFullTag() && tag.isEndTag()) {
      return false;
    }

    // Check class attribute
    PageElementTag.Parameter paramClass = tag.getParameter("class");
    if ((paramClass == null) || StringUtils.isEmpty(paramClass.getTrimmedValue())) {
      return false;
    }
    String classValue = paramClass.getTrimmedValue();
    if (!classValue.contains("mw-content-")) {
      return false;
    }
    if (!classValue.contains("mw-content-ltr") && !classValue.contains("mw-content-rtl")) {
      return false;
    }

    // Check dir or lang attribute
    PageElementTag.Parameter paramDir = tag.getParameter("dir");
    if (paramDir != null) {
      return false;
    }
    PageElementTag.Parameter paramLang = tag.getParameter("lang");
    if (paramLang != null) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    final int beginIndex = tag.getBeginIndex();
    final int endIndex = tag.getEndIndex();
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
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
}
