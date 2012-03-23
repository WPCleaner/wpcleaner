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
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 84 of check wikipedia project.
 * Error 84: Section without content
 */
public class CheckErrorAlgorithm084 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm084() {
    super("Section without content");
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

    // Retrieving titles
    boolean result = false;
    List<PageElementTitle> titles = pageAnalysis.getTitles();
    if (titles == null) {
      return false;
    }

    // Analyzing titles
    String contents = pageAnalysis.getContents();
    for (int i = 0; i < titles.size(); i++) {
      PageElementTitle title = titles.get(i);
      PageElementTitle nextTitle = (i + 1 < titles.size()) ? titles.get(i + 1) : null;
      if ((nextTitle == null) ||
          (nextTitle.getFirstLevel() <= title.getFirstLevel())) {
        boolean textFound = false;
        int lastPos = (nextTitle != null) ? nextTitle.getBeginIndex() : contents.length(); 
        int pos = title.getEndIndex();
        while (!textFound && (pos < lastPos)) {
          if (!Character.isWhitespace(contents.charAt(pos))) {
            textFound = true;
          }
          pos++;
        }
        if (!textFound) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), title.getBeginIndex(), title.getEndIndex());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
