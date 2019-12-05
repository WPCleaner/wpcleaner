/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 546 of check wikipedia project.
 * Error 546: Article without categories.
 */
public class CheckErrorAlgorithm546 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm546() {
    super("Article without categories");
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

    // Only in main name space
    if ((analysis.getPage().getNamespace() == null) ||
        (analysis.getPage().getNamespace().intValue() != Namespace.MAIN)) {
      return false;
    }

    // Check if categories are present
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories != null) && (!categories.isEmpty())) {
      return false;
    }

    // Do not report redirections
    if ((analysis.getPage().getRedirects() != null) &&
        (analysis.getPage().getRedirects().isRedirect())) {
      return false;
    }

    // Retrieve configuration
    String tmp = getSpecificProperty("templates", true, true, false);
    if ((tmp != null) && !tmp.isEmpty()) {
      List<String> categorizingTemplates = WPCConfiguration.convertPropertyToStringList(tmp);
      if (categorizingTemplates != null) {
        for (String categorizingTemplate : categorizingTemplates) {
          List<PageElementTemplate> templates = analysis.getTemplates(categorizingTemplate);
          if ((templates != null) && !templates.isEmpty()) {
            return false;
          }
        }
      }
    }

    // No categories found
    return true;
  }
}
