/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a58x.a581;

import java.util.Collection;
import java.util.List;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.TagBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 581 of check wikipedia project.
 * <br>
 * Error 581: Reference with an empty name.
 */
public class CheckErrorAlgorithm581 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm581.class);

  public CheckErrorAlgorithm581() {
    super("Reference with an empty name");
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
    if (analysis.isInNamespace(Namespace.TEMPLATE)) {
      return false;
    }

    // Check each reference
    List<PageElementTag> refTags = analysis.getCompleteTags(WikiTagType.REF);
    if (refTags == null) {
      return false;
    }
    boolean result = false;
    for (PageElementTag refTag : refTags) {
      result |= analyzeTag(analysis, errors, refTag);
    }

    return result;
  }

  /**
   * Analyze a reference tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param refTag Reference tag to analyze.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag refTag) {
    // Check if error is present
    PageElementTag.Parameter nameParam = refTag.getParameter("name");
    if (nameParam == null) {
      return false;
    }
    String name = nameParam.getTrimmedValue();
    if (name != null && !name.isEmpty()) {
      return false;
    }

    // Ignore in some situations
    if (analysis.getSurroundingTag(WikiTagType.NOWIKI, refTag.getBeginIndex()) != null) {
      return false;
    }

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(analysis, refTag.getBeginIndex(), refTag.getEndIndex());
    if (refTag.getParametersCount() == 1) {
      errorResult.addReplacement(
          TagBuilder.from(WikiTagType.REF, refTag.isEndTag(), refTag.isFullTag()).toString(),
          !refTag.isEndTag() && !refTag.isFullTag());
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
}
