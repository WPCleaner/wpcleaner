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
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 503 of check wikipedia project.
 * Error 503: Internal link in title
 */
public class CheckErrorAlgorithm503 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm503() {
    super("Internal link in title");
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

    // Check every internal link
    Collection<PageElementInternalLink> links = pageAnalysis.getInternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementInternalLink link : links) {
      PageElementTitle title = pageAnalysis.isInTitle(link.getBeginIndex());
      if (title != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult error = createCheckErrorResult(
            pageAnalysis.getPage(),
            link.getBeginIndex(), link.getEndIndex());
        error.addReplacement(link.getDisplayedText());
        errors.add(error);
      }
    }

    return result;
  }
}
