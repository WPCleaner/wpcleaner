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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 505 of check wikipedia project.
 * Error 506: Reference with a numeric name
 */
public class CheckErrorAlgorithm506 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm506() {
    super("Reference with a numeric name");
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

    // Check every ref tag
    Collection<PageElementTag> refTags = pageAnalysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if ((refTags == null) || (refTags.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTag refTag : refTags) {
      PageElementTag.Parameter paramName = refTag.getParameter("name");
      if ((paramName != null) &&
          (paramName.getValue() != null) &&
          (paramName.getValue().trim().length() > 0)) {
        String name = paramName.getValue().trim();
        boolean hasNonNumericCharacter = false;
        int index = name.length();
        while ((!hasNonNumericCharacter) && (index > 0)) {
          index--;
          if (!Character.isDigit(name.charAt(index))) {
            hasNonNumericCharacter = true;
          }
        }
        if (!hasNonNumericCharacter) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(),
              refTag.getBeginIndex(), refTag.getEndIndex());
          errors.add(error);
        }
      }
    }

    return result;
  }
}
