/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a58x.a584;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 584 of check wikipedia project.
 * <br>
 * Error 584: Line jumps before template.
 */
@SuppressWarnings("unused")
public class CheckErrorAlgorithm584 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm584() {
    super("Line jumps before template");
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
    if (templateNames.isEmpty()) {
      return false;
    }

    // Check each template
    boolean result = false;
    LineJumps lineJumps = null;
    for (PageElementTemplate template : analysis.getTemplates()) {
      if (templateNames.contains(template.getTemplateName())) {
        if (lineJumps == null) {
          lineJumps = new LineJumps(analysis);
        }
        result |= analyzeTemplate(analysis, errors, template, lineJumps);
        if (result && (errors == null)) {
          return true;
        }
      }
    }

    return result;
  }

  /**
   * Analyze a template to check if errors are present.
   *
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param template Template.
   * @param lineJumps Elements used to do line jumps.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      LineJumps lineJumps) {

    // Ignore templates inside other templates
    if (template.getBeginIndex() <= 0) {
      return false;
    }
    PageElementTemplate surroundingTemplate = analysis.isInTemplate(template.getBeginIndex() - 1);
    if ((surroundingTemplate != null) && (surroundingTemplate.getEndIndex() > template.getBeginIndex())) {
      return false;
    }

    // Move backward through whitespace and elements used to do line jumps
    String contents = analysis.getContents();
    int beginIndex = template.getBeginIndex();
    int newLineCount = 0;
    boolean lineJumpElement = false;
    boolean tryAgain = true;
    while (tryAgain && (beginIndex > 0)) {
      tryAgain = false;
      char previousChar = contents.charAt(beginIndex - 1);
      if (previousChar == '\n') {
        newLineCount++;
        beginIndex--;
        tryAgain = true;
      } else if ((previousChar == ' ') || (previousChar == '\t')) {
        beginIndex--;
        tryAgain = true;
      } else {
        Integer elementBeginIndex = lineJumps.elementsByEndIndex.get(beginIndex);
        if (elementBeginIndex != null) {
          lineJumpElement = true;
          beginIndex = elementBeginIndex;
          tryAgain = true;
        }
      }
    }

    // Keep the template at the beginning of the page or its line
    if (beginIndex == 0) {
      return false;
    }
    if (newLineCount == 0) {
      return false;
    }
    if (!lineJumpElement && (newLineCount <= 2)) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int endIndex;
    if (template.getParameterCount() == 0) {
      endIndex = template.getEndIndex();
    } else {
      endIndex = template.getParameterPipeIndex(0) + 1;
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    String replacement = "\n\n" + contents.substring(template.getBeginIndex(), endIndex);
    errorResult.addReplacement(replacement, false);
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

  /**
   * Elements used to do line jumps in a page.
   */
  private class LineJumps {

    /** Begin index of elements used to do line jumps, indexed by their end index */
    final Map<Integer, Integer> elementsByEndIndex = new HashMap<>();

    LineJumps(PageAnalysis analysis) {
      for (PageElementTag tag : analysis.getTags(HtmlTagType.BR)) {
        elementsByEndIndex.put(tag.getEndIndex(), tag.getBeginIndex());
      }
      if (!lineJumpTemplateNames.isEmpty()) {
        for (PageElementTemplate template : analysis.getTemplates()) {
          if (lineJumpTemplateNames.contains(template.getTemplateName())) {
            elementsByEndIndex.put(template.getEndIndex(), template.getBeginIndex());
          }
        }
      }
    }
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates that shouldn't be preceded by multiple line jumps */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Templates used to do line jumps */
  private static final String PARAMETER_LINE_JUMP_TEMPLATES = "line_jump_templates";

  /**
   * Initialize settings for the algorithm.
   *
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    TemplateConfigurationGroup group = new TemplateConfigurationGroup();
    List<String[]> generalList = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.TEMPLATE_GROUPS);
    if (generalList != null) {
      group.addGroups(generalList);
    }

    initializeTemplateNames(group, PARAMETER_TEMPLATES, templateNames);
    initializeTemplateNames(group, PARAMETER_LINE_JUMP_TEMPLATES, lineJumpTemplateNames);
  }

  /**
   * Initialize a list of template names from a parameter.
   *
   * @param group Template groups.
   * @param parameter Parameter name.
   * @param names List of template names to initialize.
   */
  private void initializeTemplateNames(
      TemplateConfigurationGroup group,
      String parameter,
      Set<String> names) {
    String tmp = getSpecificProperty(parameter, true, true, false);
    names.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        tmpList.forEach(item -> group.getTemplateNames(item).forEach(
            templateName -> names.add(Page.normalizeTitle(templateName))));
      }
    }
  }

  /** Templates that shouldn't be preceded by multiple line jumps */
  private final Set<String> templateNames = new HashSet<>();

  /** Templates used to do line jumps */
  private final Set<String> lineJumpTemplateNames = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that shouldn't be preceded by multiple line jumps"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Template that shouldn't be preceded by multiple line jumps"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_LINE_JUMP_TEMPLATES,
        GT._T("Templates used to do line jumps"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Template used to do line jumps"))
        },
        true));
  }
}
