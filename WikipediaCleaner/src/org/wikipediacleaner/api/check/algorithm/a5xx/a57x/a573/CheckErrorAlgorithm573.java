/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a573;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 573 of check wikipedia project.
 * <br>
 * Error 573: Unnecessary non-breaking space <code>&amp;nbsp;</code>.
 */
public class CheckErrorAlgorithm573 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm573() {
    super("Unnecessary non-breaking space");
  }

  // See https://fr.wikipedia.org/wiki/Wikipedia:TYPO#ESPACES
  private static final String PUNCTUATION_AFTER = "»;:?!%";
  private static final String PUNCTUATION_BEFORE = "«";

  private static final List<String> NBSP = Stream.of("&nbsp;").collect(Collectors.toList());

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

    // Check each non-breaking space character
    boolean result = false;
    String contents = analysis.getContents();
    int index = 0;
    while (index < contents.length()) {
      boolean found = false;
      for (String nbsp : getNonBreakingSpaceTexts()) {
        if (!found && contents.startsWith(nbsp, index)) {
          result |= analyzePosition(analysis, errors, index, nbsp);
          found = true;
          index += nbsp.length();
        }
      }
      if (!found) {
        index++;
      }
    }

    return result;
  }

  /**
   * @return List of texts matching a non-breaking space
   */
  protected List<String> getNonBreakingSpaceTexts() {
    return NBSP;
  }

  /**
   * Analyze a position to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param index Index of the position to check.
   * @param nbsp Text for the non-breaking space.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzePosition(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int index,
      String nbsp) {

    // Extend area
    String contents = analysis.getContents();
    int beginIndex = index;
    int endIndex = index + nbsp.length();
    int quotesAfter = ContentsUtil.moveIndexForwardWhileFound(contents, endIndex, "'") - endIndex;
    int quotesBefore = beginIndex - ContentsUtil.moveIndexBackwardWhileFound(contents, beginIndex - 1, "'") - 1;
    if ((quotesAfter != quotesBefore) ||
        ((quotesAfter != 2) && (quotesAfter != 3) && (quotesAfter != 5))) {
      quotesAfter = 0;
      quotesBefore = 0;
    }
    beginIndex -= quotesBefore;
    endIndex += quotesAfter;

    // Check if the non-breaking space is useless
    boolean useless = false;
    beginIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, beginIndex - 1, " ") + 1;
    if ((beginIndex > 0) &&
        (PUNCTUATION_BEFORE.indexOf(contents.charAt(beginIndex - 1)) >= 0)) {
      useless = true;
    }
    endIndex = ContentsUtil.moveIndexForwardWhileFound(contents, endIndex, " ");
    if ((endIndex < contents.length()) &&
        (PUNCTUATION_AFTER.indexOf(contents.charAt(endIndex)) >= 0)) {
      if ((beginIndex > 0) &&
          (contents.charAt(beginIndex - 1) != '\n') &&
          !contents.startsWith("!!", endIndex)) {
        useless = true;
      }
    }
    if (!useless) {
      return false;
    }

    // Ignore specific situations
    if (!ignoreTemplates.isEmpty()) {
      PageElementTemplate template = analysis.isInTemplate(index);
      if (template != null) {
        Parameter parameter = template.getParameterAtIndex(index);
        Set<String> parameters = ignoreTemplates.get(template.getTemplateName());
        if ((parameter != null) &&
            (parameters != null) &&
            (parameters.contains(parameter.getComputedName()))) {
          return false;
        }
      }
    }

    if (errors == null) {
      return true;
    }

    // Decide if modifications can be automatic
    boolean automatic = true;
    if ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '\'') &&
        (endIndex < contents.length()) && (contents.charAt(endIndex) == '\'')) {
      automatic = false;
    }

    // Report error
    int fullBeginIndex = Math.max(0, beginIndex - 2);
    int fullEndIndex = Math.min(endIndex + 2, contents.length());
    CheckErrorResult errorResult = createCheckErrorResult(analysis, fullBeginIndex, fullEndIndex);
    errorResult.addReplacement(
        contents.substring(fullBeginIndex, beginIndex) + " " + contents.substring(endIndex, fullEndIndex),
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
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates and parameters that are ignored */
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
      for (String[] template : tmpList) {
        if (template.length > 0) {
          Set<String> parameters = ignoreTemplates.computeIfAbsent(template[0], key -> new HashSet<>());
          for (int index = 1; index < template.length; index++) {
            parameters.add(template[index]);
          }
        }
      }
    }
  }

  /** Templates and parameters that are ignored */
  private final Map<String, Set<String>> ignoreTemplates = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_TEMPLATES,
        GT._T("Templates to ignore"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "group",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
  }
}
