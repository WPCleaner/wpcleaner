/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 502 of check wikipedia project.
 * Error 502: Template namespace in template usage
 */
public class CheckErrorAlgorithm502 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm502() {
    super("Template namespace in template usage");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieve informations
    WikiConfiguration config = pageAnalysis.getWikiConfiguration();
    Namespace templateNamespace = config.getNamespace(Namespace.TEMPLATE);

    // Check every template to if template name contains template namespace
    Collection<PageElementTemplate> templates = pageAnalysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTemplate template : templates) {
      String templateName = template.getTemplateName();
      int colonIndex = templateName.indexOf(':');
      String namespace = (colonIndex > 0) ? templateName.substring(0, colonIndex) : null;
      if ((colonIndex > 0) &&
          (templateNamespace.isPossibleName(namespace))) {

        // Test for special situations (functions)
        MagicWord magicWord = null;
        if (templateName.length() > colonIndex + 1) {
          String afterColon = templateName.substring(colonIndex + 1);
          colonIndex = afterColon.indexOf(':');
          if (colonIndex < 0) {
            magicWord = config.getFunctionMagicWord(afterColon, false);
          } else {
            magicWord = config.getFunctionMagicWord(afterColon.substring(0, colonIndex), true);
          }
        }

        // Raise error
        if (magicWord == null) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(),
              template.getBeginIndex(),
              template.getEndIndex());
          String fullTemplate = pageAnalysis.getContents().substring(
              template.getBeginIndex(), template.getEndIndex());
          colonIndex = fullTemplate.indexOf(':');
          error.addReplacement(
              "{{" + fullTemplate.substring(colonIndex + 1).trim(),
              GT._("Remove {0} namespace from template name", namespace),
              true);
          errors.add(error);
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
  public String automaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
