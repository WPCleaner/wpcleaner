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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 508 of check wikipedia project.
 * Error 508: Missing template
 */
public class CheckErrorAlgorithm508 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm508() {
    super("Missing template");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((pageAnalysis == null) ||
        (pageAnalysis.getPage() == null) ||
        (pageAnalysis.getPage().getTemplates() == null)) {
      return false;
    }

    // Analyze each template
    Namespace templateNS = pageAnalysis.getWikiConfiguration().getNamespace(Namespace.TEMPLATE);
    List<PageElementTemplate> templates = pageAnalysis.getTemplates();
    boolean result = false;
    for (PageElementTemplate template : templates) {
      String templateName = templateNS.getTitle() + ":" + template.getTemplateName();
      boolean missing = false;
      for (Page templatePage : pageAnalysis.getPage().getTemplates()) {
        if (Page.areSameTitle(templateName, templatePage.getTitle())) {
          if (Boolean.FALSE.equals(templatePage.isExisting())) {
            missing = true;
          }
        }
      }

      if (missing) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), template.getBeginIndex(), template.getEndIndex());
        errors.add(errorResult);
      }
    }
    return result;
  }
}
