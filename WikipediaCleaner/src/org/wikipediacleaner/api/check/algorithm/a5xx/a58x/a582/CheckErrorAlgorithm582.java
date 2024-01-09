/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a58x.a582;

import java.util.Collection;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 582 of check wikipedia project.
 * <br>
 * Error 582: Replacement character.
 */
public class CheckErrorAlgorithm582 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm582.class);

  public CheckErrorAlgorithm582() {
    super("Replacement character");
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

    // Check each character
    String contents = analysis.getContents();
    boolean result = false;
    int currentIndex = 0;
    for (;;) {
      currentIndex = contents.indexOf('\uFFFD', currentIndex);
      if (currentIndex < 0) {
        return result;
      }
      result = true;
      currentIndex = reportError(analysis, errors, currentIndex);
    }
  }

  /**
   * Report error.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param index Index of current error.
   * @return Flag indicating if the error was found.
   */
  private int reportError(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int index) {
    
    // Report error in template
    PageElementTemplate template = analysis.isInTemplate(index);
    if (template != null) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, template.getBeginIndex(), template.getEndIndex());
      errors.add(errorResult);
      return template.getEndIndex();
    }

    // Report error in other cases
    CheckErrorResult errorResult = createCheckErrorResult(analysis, index, index + 1);
    errors.add(errorResult);
    return index + 1;
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
