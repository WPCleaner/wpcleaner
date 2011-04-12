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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 76 of check wikipedia project.
 * Error 76: Link with no space
 */
public class CheckErrorAlgorithm076 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm076() {
    super("Link with no space");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      PageElementInternalLink link = PageContents.findNextInternalLink(page, contents, startIndex);
      if (link != null) {
        startIndex = link.getEndIndex();
        int spaceIndex = link.getFullLink().indexOf("%20");
        if (spaceIndex >= 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          StringBuilder sb = new StringBuilder();
          sb.append("[[");
          sb.append(link.getFullLink().replaceAll("\\%20", " "));
          if (link.getText() != null) {
            sb.append("|");
            sb.append(link.getText());
          }
          sb.append("]]");
          CheckErrorResult errorResult = createCheckErrorResult(
              page, link.getBeginIndex(), link.getEndIndex());
          errorResult.addReplacement(
              sb.toString(),
              GT._("Replace %20 by space character"));
          errors.add(errorResult);
        }
      } else {
        startIndex = contents.length();
      }
    }

    return result;
  }
}
