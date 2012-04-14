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

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;
import org.wikipediacleaner.utils.TextProviderUrlTitle;


/**
 * Algorithm for analyzing error 79 of check wikipedia project.
 * Error 79: External link without description.
 */
public class CheckErrorAlgorithm079 extends CheckErrorAlgorithmBase {

  /**
   * StringChecker for the description.
   */
  private final StringChecker descriptionChecker;

  public CheckErrorAlgorithm079() {
    super("External link without description");
    descriptionChecker = new StringCheckerUnauthorizedCharacters("[]");
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

    boolean result = false;
    Collection<PageElementExternalLink> links = pageAnalysis.getExternalLinks();
    for (PageElementExternalLink link : links) {
      String text = link.getText();
      if ((text == null) || (text.trim().length() == 0)) {
        PageElementTag refTag = pageAnalysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_REF, link.getBeginIndex());
        if ((refTag == null) || (link.hasSquare())) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
          String url = link.getLink();
          errorResult.addPossibleAction(
              GT._("Add a description..."),
              new AddTextActionProvider(
                  "[" + url + " ", "]",
                  new TextProviderUrlTitle(url),
                  GT._("What description would like to use for the external link ?"),
                  descriptionChecker));
          if (refTag == null) {
            errorResult.addReplacement(
                "<ref>" + url + "</ref>",
                GT._("Convert into <ref> tag"));
            errorResult.addPossibleAction(
                GT._("Add a description and convert into <ref> tag"),
                new AddTextActionProvider(
                    "<ref>[" + url + " ", "]</ref>",
                    new TextProviderUrlTitle(url),
                    GT._("What description would like to use for the external link ?"),
                    descriptionChecker));
          } else if (link.hasSquare()){
            errorResult.addReplacement(url);
          }
          errorResult.addPossibleAction(
              new SimpleAction(GT._("External viewer"),
                  new PageViewAction(url)));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
