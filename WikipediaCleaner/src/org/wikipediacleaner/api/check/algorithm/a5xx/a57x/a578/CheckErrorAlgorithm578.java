/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a578;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 578 of check wikipedia project.
 * <br>
 * Error 578: Template in list.
 */
public class CheckErrorAlgorithm578 extends CheckErrorAlgorithmBase {

  @Nonnull private static final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm578.class);

  public CheckErrorAlgorithm578() {
    super("Template in list");
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

    // Check each template
    boolean result = false;
    for (Map.Entry<String, Boolean> templateConfiguration : templateNames.entrySet()) {
      List<PageElementTemplate> currentTemplates = analysis.getTemplates(templateConfiguration.getKey());
      for (PageElementTemplate template : currentTemplates) {
        result |= analyzeTemplate(analysis, errors, template, templateConfiguration.getValue());
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
   * @param ignoreAfter True if text after should be ignored.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeTemplate(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate template,
      Boolean ignoreAfter) {

    // Check if template is in list item
    PageElementListItem listItem = analysis.isInListItem(template.getBeginIndex());
    if (listItem == null) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int beginIndex = listItem.getBeginIndex();
    int endIndex;
    if (template.getParameterCount() == 0) {
      endIndex = template.getEndIndex() - 2;
    } else {
      endIndex = template.getParameterPipeIndex(0);
    }
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    String contents = analysis.getContents();
    int tmpBeginIndex = ContentsUtil.moveIndexAfterWhitespace(contents, beginIndex + listItem.getDepth());
    boolean automatic = true;
    if (tmpBeginIndex < template.getBeginIndex()) {
      automatic = false;
    }
    if ((listItem.getEndIndex() > template.getEndIndex()) && Boolean.FALSE.equals(ignoreAfter)) {
      automatic = false;
    }
    if (listItem.getDepth() > 1) {
      automatic = false;
    }
    if (automatic && (beginIndex > 0)) {
      PageElementListItem previousItem = analysis.isInListItem(beginIndex - 1);
      if (previousItem != null) {
        automatic = false;
      }
    }
    errorResult.addReplacement(contents.substring(tmpBeginIndex, endIndex), automatic);
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

  /** Templates that shouldn't be used in list item */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templateNames.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      for (String[] tmpElement : tmpList) {
        Boolean ignoreAfter = tmpElement.length > 1 ? Boolean.valueOf(tmpElement[1]) : Boolean.FALSE;
        templateNames.put(Page.normalizeTitle(tmpElement[0]), ignoreAfter);
      }
    }
  }

  /** Templates that shouldn't be used in list item */
  private final Map<String, Boolean> templateNames = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that shouldn't be used in list item"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "templates",
                GT._T("Template that shouldn't be used in list item"))
        },
        true));
  }
}
