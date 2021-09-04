/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a9xx.a99x.a999;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;

/**
 * Algorithm for analyzing error 999 of check wikipedia project.
 * Error 999: Test for WPCleaner
 */
public class CheckErrorAlgorithm999 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm999() {
    super("Test for WPCleaner");
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

    boolean result = false;
    result |= analyzeTags(analysis, errors);

    return result;
  }

  /**
   * Analyze tags in a page to check there are errors.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementTag> tags = analysis.getTags();
    if ((tags == null) || tags.isEmpty()) {
      return false;
    }

    // Check each cite tag
    boolean result = false;
    for (PageElementTag tag : tags) {
      result |= analyzeTag(analysis, errors, tag);
    }

    return result;
  }

  private static final Pattern patternSplitClasses = Pattern.compile(" ");

  /**
   * Analyze a tag to check if there's an error.
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

    // Ignore end tags
    if (tag.isEndTag()) {
      return false;
    }

    // Ignore non HTML tags
    if (!(tag.getType() instanceof HtmlTagType)) {
      return false;
    }

    // Ignore tags without class attribute
    Parameter classParam = tag.getParameter("class");
    if ((classParam == null) || StringUtils.isEmpty(classParam.getValue())) {
      return false;
    }

    String[] classes = patternSplitClasses.split(classParam.getValue());
    for (String clazz : classes) {
      if (Objects.equals("citation", clazz)) {
        if (errors == null) {
          return true;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
        errors.add(errorResult);
        return true;
      }
    }

    return false;
  }
}
