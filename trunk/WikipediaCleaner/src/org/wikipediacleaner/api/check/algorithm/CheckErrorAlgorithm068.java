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


import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 68 of check wikipedia project.
 * Error 68: Link to other language
 */
public class CheckErrorAlgorithm068 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm068() {
    super("Link to other language");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Retrieve possible templates to replace the link to other language
    String templatesParam = page.getWikipedia().getCheckWikiProperty(
        "template", 68, true, false, false);
    String[] templatesList = null;
    if (templatesParam != null) {
      templatesList = page.getWikipedia().convertPropertyToStringArray(templatesParam);
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      PageElementInternalLink link = PageContents.findNextInternalLink(page, contents, startIndex);
      if (link != null) {
        boolean found = false;

        // Check that link starts with :
        String linkUrl = link.getLink();
        if ((linkUrl != null) && (linkUrl.startsWith(":"))) {

          int currentPos = linkUrl.indexOf(":", 1);
          if (currentPos > 1) {
            String namespace = linkUrl.substring(1, currentPos);
            for (Language lg : page.getWikipedia().getLanguages()) {
              if (namespace.equals(lg.getCode())) {
                if (errors == null) {
                  return true;
                }
                result = true;
                CheckErrorResult errorResult = createCheckErrorResult(
                    page, link.getBeginIndex(), link.getEndIndex());
                EnumWikipedia fromWikipedia = EnumWikipedia.getWikipedia(lg.getCode());
                if (fromWikipedia != null) {
                  String pageTitle = linkUrl.substring(currentPos + 1);
                  errorResult.addPossibleAction(
                      GT._("Check language links"),
                      new CheckLanguageLinkActionProvider(
                          fromWikipedia, page.getWikipedia(),
                          pageTitle));
                  if ((templatesList != null) && (templatesList.length > 0)) {
                    for (String template : templatesList) {
                      String[] templateArgs = template.split("\\|");
                      if (templateArgs.length >= 5) {
                        errorResult.addPossibleAction(
                            GT._("Replace using template {0}", "{{" + templateArgs[0] + "}}"),
                            new AddTextActionProvider(
                                "{{" + templateArgs[0] +
                                "|" + templateArgs[1] + "=",
                                "|" + templateArgs[2] + "=" + lg.getCode() +
                                "|" + templateArgs[3] + "=" + pageTitle +
                                "|" + templateArgs[4] + "=" + link.getDisplayedText() +
                                "}}",
                                null,
                                GT._("What is the title of the page on this wiki ?"),
                                (link.getText() != null) ? link.getText() : pageTitle,
                                "[]\""));
                      }
                    }
                  }
                  errorResult.addPossibleAction(
                      GT._("External Viewer"),
                      new BasicActionProvider(
                          new PageViewAction(pageTitle, fromWikipedia)));
                }
                errors.add(errorResult);
              }
            }
          }
        }
        if (found) {
          startIndex = link.getEndIndex();
        } else {
          startIndex = link.getBeginIndex() + 2;
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("template", GT._(
        "A template that can be used instead of the link to an other language. " +
        "It must be specified as: " +
          "<template name>|" +
          "<param name for local page name>|" +
          "<param name for code of other language>|" +
          "<param name for page name in other language>|" +
          "<param name for displayed text>").replaceAll("\\<", "&lt;").replaceAll("\\>", "&gt;"));
    return parameters;
  }
}
