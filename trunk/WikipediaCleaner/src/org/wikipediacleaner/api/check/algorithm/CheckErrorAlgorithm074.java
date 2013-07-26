/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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
import org.wikipediacleaner.api.data.PageElementInternalLink;


/**
 * Algorithm for analyzing error 74 of check wikipedia project.
 * Error 74: Link with no target
 */
public class CheckErrorAlgorithm074 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm074() {
    super("Link with no target");
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
    for (PageElementInternalLink link : pageAnalysis.getInternalLinks()) {
      if (link.getFullLink().trim().length() == 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
        String text = link.getText();
        if ((text != null) && (text.length() > 0)) {
          errorResult.addReplacement("[[" + link.getText() + "]]");
        }
        errors.add(errorResult);
      }
    }

    return result;
  }
}
