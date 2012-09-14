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
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 504 of check wikipedia project.
 * Error 504: Reference in title
 */
public class CheckErrorAlgorithm504 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm504() {
    super("Reference in title");
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

    // Check every reference
    Collection<PageElementTag> refs = pageAnalysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTag ref : refs) {
      PageElementTitle title = pageAnalysis.isInTitle(ref.getCompleteBeginIndex());
      if (title != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult error = createCheckErrorResult(
            pageAnalysis.getPage(),
            ref.getCompleteBeginIndex(), ref.getCompleteEndIndex());
        errors.add(error);
      }
    }

    return result;
  }
}
