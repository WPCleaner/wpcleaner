/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
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

    // Retrieve template namespace
    Namespace templateNamespace = Namespace.getNamespace(
        Namespace.TEMPLATE,
        pageAnalysis.getWikipedia().getNamespaces());

    // Check every template
    Collection<PageElementTemplate> templates = pageAnalysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTemplate template : templates) {
      // Check if template name contains template namespace
      String templateName = template.getTemplateName();
      int colonIndex = templateName.indexOf(':');
      if ((colonIndex > 0) &&
          (templateNamespace.isPossibleName(templateName.substring(0, colonIndex)))) {
        if (errors == null) {
          return true;
        }
        result = true;
        String namespace = templateName.substring(0, colonIndex);
        CheckErrorResult error = createCheckErrorResult(
            pageAnalysis.getPage(),
            template.getBeginIndex(),
            template.getEndIndex());
        String fullTemplate = pageAnalysis.getContents().substring(
            template.getBeginIndex(), template.getEndIndex());
        colonIndex = fullTemplate.indexOf(':');
        error.addReplacement(
            "{{" + fullTemplate.substring(colonIndex + 1),
            GT._("Remove {0} namespace from template name", namespace));
        errors.add(error);
      }
    }

    return result;
  }
}
