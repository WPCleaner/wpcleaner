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
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementLanguageLink;


/**
 * Algorithm for analyzing error 53 of check wikipedia project.
 * Error 53: Interwiki before last category
 */
public class CheckErrorAlgorithm053 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm053() {
    super("Interwiki before last category");
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

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      PageElementLanguageLink link = PageContents.findNextLanguageLink(
          pageAnalysis.getPage(), contents, startIndex);
      if (link != null) {
        startIndex = link.getEndIndex();
        PageElementCategory category = PageContents.findNextCategory(
            pageAnalysis.getPage(), contents, startIndex, pageAnalysis.getComments());
        if (category != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
          errors.add(errorResult);
        } else {
          return result;
        }
      } else {
        startIndex = contents.length();
      }
    }

    return result;
  }
}
