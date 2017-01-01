/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

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
    String templatesProperty = getSpecificProperty("templates", true, true, false);
    List<String[]> prohibitedTemplates = null;
    if (templatesProperty != null) {
      prohibitedTemplates = WPCConfiguration.convertPropertyToStringArrayList(templatesProperty);
    }
    if ((prohibitedTemplates != null) && !prohibitedTemplates.isEmpty()) {
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
    }

    return result;
  }


  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("templates", GT._("A list of templates that can't be used inside a reference"));
    return parameters;
  }
}
