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
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 83 of check wikipedia project.
 * Error 83: Headlines start with three "=" and later with level two
 */
public class CheckErrorAlgorithm083 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm083() {
    super("Headlines start with three \"=\" and later with level two");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments in the page contents.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    PageElementTitle firstTitle = PageContents.findNextTitle(page, contents, 0, comments);
    if (firstTitle == null) {
      return false;
    }
    if (firstTitle.getFirstLevel() < 3) {
      return false;
    }
    int startIndex = firstTitle.getEndIndex();
    while (startIndex < contents.length()) {
      PageElementTitle title = PageContents.findNextTitle(page, contents, startIndex, comments);
      if (title == null) {
        startIndex = contents.length();
      } else {
        if (title.getFirstLevel() < 3) {
          if (errors == null) {
            return true;
          }
          errors.add(createCheckErrorResult(page, firstTitle.getBeginIndex(), firstTitle.getEndIndex()));
          return true;
        }
        startIndex = title.getEndIndex();
      }
    }
    return false;
  }
}
