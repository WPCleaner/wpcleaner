/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a55x.a552;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 552 of check wikipedia project.
 * Error 552: Template ending with }}}.
 */
public class CheckErrorAlgorithm552 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm552() {
    super("Template ending with }}}");
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

    // Check each kind of element
    boolean result = false;
    result |= analyzeTemplates(analysis, errors);
    result |= analyzeFunctions(analysis, errors);
    return result;
  }

  /**
   * Analyze a page to check if errors are present in templates.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeTemplates(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementTemplate> templates = analysis.getTemplates();
    if (templates == null) {
      return false;
    }

    // Check each template
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTemplate template : templates) {
      int endIndex = template.getEndIndex();
      if ((endIndex < contents.length()) && (contents.charAt(endIndex) == '}')) {

        // Check if there's something explaining this extra bracket
        boolean foundReason = false;
        if (!foundReason) {
          PageElementTemplate otherTemplate = analysis.isInTemplate(endIndex);
          if ((otherTemplate != null) &&
              (otherTemplate.getEndIndex() == endIndex + 2)) {
            foundReason = true;
          }
        }
        if (!foundReason) {
          PageElementFunction function = analysis.isInFunction(endIndex);
          if ((function != null) &&
              (function.getEndIndex() == endIndex + 2)) {
            foundReason = true;
          }
        }
        if (!foundReason) {
          foundReason = ignoreTemplates.contains(template.getTemplateName());
        }

        // Report error
        if (!foundReason) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Count brackets
          int openingBracketsInside = 0;
          int closingBracketsInside = 0;
          if (template.getParameterCount() > 0) {
            openingBracketsInside = ContentsUtil.countCharacters(
                contents, template.getParameterNameStartIndex(0), endIndex - 2, "{");
            closingBracketsInside = ContentsUtil.countCharacters(
                contents, template.getParameterNameStartIndex(0), endIndex - 2, "}");
          }
          int beginLine = ContentsUtil.getLineBeginIndex(contents, template.getBeginIndex());
          int endLine = ContentsUtil.getLineEndIndex(contents, template.getEndIndex());
          int openingBrackets = ContentsUtil.countCharacters(contents, beginLine, endLine, "{");
          int closingBrackets = ContentsUtil.countCharacters(contents, beginLine, endLine, "}");

          // Report error
          int beginIndex = template.getBeginIndex();
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex + 1);
          if (openingBracketsInside > closingBracketsInside) {
            String replacement =
                contents.substring(beginIndex, endIndex - 1) +
                WikiTagType.NOWIKI.getFullTag() +
                contents.substring(endIndex - 1, endIndex + 1);
            errorResult.addReplacement(replacement, "{{...}<nowiki/>}}");
          }
          if (openingBrackets == closingBrackets) {
            String replacement =
                contents.substring(beginIndex, endIndex) +
                WikiTagType.NOWIKI.getFullTag() +
                contents.substring(endIndex, endIndex + 1);
            errorResult.addReplacement(replacement, "{{...}}<nowiki/>}");
          }
          errorResult.addReplacement(
              contents.substring(beginIndex, endIndex),
              "{{...}}");
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present in functions.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeFunctions(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    List<PageElementFunction> functions = analysis.getFunctions();
    if (functions == null) {
      return false;
    }

    // Check each function
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementFunction function : functions) {
      int endIndex = function.getEndIndex();
      if ((endIndex < contents.length()) &&
          (contents.charAt(endIndex) == '}') &&
          (contents.startsWith("{{", function.getBeginIndex()))) {

        // Check if there's something explaining this extra bracket
        boolean foundReason = false;
        if (!foundReason) {
          PageElementTemplate otherTemplate = analysis.isInTemplate(endIndex);
          if ((otherTemplate != null) &&
              (otherTemplate.getEndIndex() == endIndex + 2)) {
            foundReason = true;
          }
        }
        if (!foundReason) {
          PageElementFunction otherFunction = analysis.isInFunction(endIndex);
          if ((otherFunction != null) &&
              (otherFunction.getEndIndex() == endIndex + 2)) {
            foundReason = true;
          }
        }

        // Report error
        if (!foundReason) {
          if (errors == null) {
            return true;
          }
          result = true;
          int beginIndex = function.getBeginIndex();
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex + 1);
          errorResult.addReplacement(
              contents.substring(beginIndex, endIndex),
              "{{...}}");
          errors.add(errorResult);
        }
      }
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
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of templates to be ignored */
  private static final String PARAMETER_IGNORE_TEMPLATES = "ignore_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_TEMPLATES, true, true, true);
    ignoreTemplates.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        ignoreTemplates.addAll(tmpList);
      }
    }
  }

  /** Templates to ignore */
  private final Set<String> ignoreTemplates = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_TEMPLATES,
        GT._T("Templates to ignore"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template to ignore")),
        true));
  }
}
