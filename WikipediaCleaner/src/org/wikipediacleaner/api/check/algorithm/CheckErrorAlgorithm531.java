/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 531 of check wikipedia project.
 * Error 531: Reference inside reference
 */
public class CheckErrorAlgorithm531 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm531() {
    super("Reference inside reference");
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

    // Analyze each reference tag
    boolean result = false;
    List<PageElementTag> refTags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if ((refTags == null) || (refTags.isEmpty())) {
      return false;
    }
    int maxIndex = 0;
    for (PageElementTag refTag : refTags) {
      if (refTag.getBeginIndex() < maxIndex) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, refTag.getCompleteBeginIndex(), refTag.getCompleteEndIndex());
        errors.add(errorResult);
      }
      if (refTag.isComplete() && (refTag.getCompleteEndIndex() > maxIndex)) {
        maxIndex = refTag.getCompleteEndIndex();
      }
    }

    // Check for prohibited templates inside reference tag
    for (String[] prohibitedTemplate : prohibitedTemplates) {
      if (prohibitedTemplate.length > 0) {
        List<PageElementTemplate> templates = analysis.getTemplates(prohibitedTemplate[0]);
        for (PageElementTemplate template : templates) {
          if (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, template.getBeginIndex()) != null) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, template.getBeginIndex(), template.getEndIndex());
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }


  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates that can't be used inside a reference */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    prohibitedTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        prohibitedTemplates.addAll(tmpList);
      }
    }
  }

  /** Templates that can't be used inside a reference */
  private final List<String[]> prohibitedTemplates = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("A list of templates that can't be used inside a reference"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of a template that can't be used inside a reference"))
        },
        true));
  }
}
