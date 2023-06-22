/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a089;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 89 of check wikipedia project.
 * Error 89: DEFAULTSORT with no space after the comma
 */
public class CheckErrorAlgorithm089 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm089() {
    super("DEFAULTSORT with no space after the comma");
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

    // Analyze each default sort
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts == null) || (defaultSorts.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementFunction defaultSort : defaultSorts) {
      result |= analyzeDefaultSort(analysis, errors, defaultSort);
    }

    return result;
  }

  public boolean analyzeDefaultSort(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementFunction defaultSort) {
    if (defaultSort.getParameterCount() <= 0) {
      return false;
    }

    String value = defaultSort.getParameterValue(0);
    if ((value == null) || (value.trim().length() == 0)) {
      return false;
    }
    value = value.trim();
    int commaIndex = value.indexOf(',');
    if ((commaIndex <= 0) || (commaIndex >= value.length() - 1)) {
      return false;
    }
    if (!Character.isLetter(value.charAt(commaIndex + 1))) {
      return false;
    }

    if (errors == null) {
      return true;
    }

    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        defaultSort.getBeginIndex(), defaultSort.getEndIndex());
    String replacement = value.substring(0, commaIndex).trim() + ", " + value.substring(commaIndex + 1).trim();
    final String pageTitle = analysis.getPage().getTitle();
    final String testValue = value.substring(commaIndex + 1).trim() + " " + value.substring(0, commaIndex).trim();
    boolean automatic = pageTitle.equalsIgnoreCase(testValue) ||
        SpecialCharacters.replaceAllSpecialCharacters(pageTitle, analysis.getWikipedia()).equalsIgnoreCase(testValue);
    errorResult.addReplacement(PageElementFunction.createFunction(
        defaultSort.getFunctionName(),
        replacement.toString()),
        automatic);
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
    if (!analysis.getPage().isArticle() ||
        analysis.getPage().isInUserNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
