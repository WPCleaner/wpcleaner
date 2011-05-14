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
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 7 of check wikipedia project.
 * Error 7: Headlines all start with three "="
 */
public class CheckErrorAlgorithm007 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm007() {
    super("Headlines all start with three \"=\"");
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
    String contents = pageAnalysis.getContents();
    PageElementTitle firstTitle = PageContents.findNextTitle(
        pageAnalysis.getWikipedia(), contents, 0, pageAnalysis.getComments());
    if (firstTitle == null) {
      return false;
    }
    if (firstTitle.getFirstLevel() < 3) {
      return false;
    }
    int startIndex = firstTitle.getEndIndex();
    while (startIndex < contents.length()) {
      PageElementTitle title = PageContents.findNextTitle(
          pageAnalysis.getWikipedia(), contents, startIndex, pageAnalysis.getComments());
      if (title == null) {
        startIndex = contents.length();
      } else {
        if (title.getFirstLevel() < 3) {
          return false;
        }
        startIndex = title.getEndIndex();
      }
    }
    if (errors == null) {
      return true;
    }
    errors.add(createCheckErrorResult(
        pageAnalysis.getPage(), firstTitle.getBeginIndex(), firstTitle.getEndIndex()));
    return true;
  }
}
