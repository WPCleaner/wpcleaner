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
import org.wikipediacleaner.api.data.PageElementTitle;

/**
 * Algorithm for analyzing error 25 of check wikipedia project.
 * Error 25: Headline hierarchy
 */
public class CheckErrorAlgorithm025 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm025() {
    super("Headline hierarchy");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    int previousTitleLevel = -1;
    while (startIndex < contents.length()) {
      PageElementTitle title = PageContents.findNextTitle(page, contents, startIndex, null);
      if (title == null) {
        startIndex = contents.length();
      } else {
        if ((previousTitleLevel > 0) &&
            (title.getFirstLevel() > previousTitleLevel + 1)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              page, title.getBeginIndex(), title.getEndIndex());
          errors.add(errorResult);
        }
        previousTitleLevel = title.getFirstLevel();
        startIndex = title.getEndIndex();
      }
    }
    return result;
  }
}
