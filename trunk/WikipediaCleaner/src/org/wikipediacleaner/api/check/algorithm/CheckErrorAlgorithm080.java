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
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;


/**
 * Algorithm for analyzing error 80 of check wikipedia project.
 * Error 80: External link with line break.
 */
public class CheckErrorAlgorithm080 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm080() {
    super("External link with line break");
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

    // Check every external links
    Collection<PageElementExternalLink> links = pageAnalysis.getExternalLinks();
    String contents = pageAnalysis.getContents();
    boolean result = false;
    for (PageElementExternalLink link : links) {
      String text = link.getTextNotTrimmed();
      if (text != null) {
        int crIndex = text.indexOf('\n');
        if (crIndex >= 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          int beginIndex = link.getBeginIndex();
          PageElementTag refTag = pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, beginIndex);
          if ((refTag != null) &&
              (refTag.getMatchingTag() != null) &&
              (refTag.getMatchingTag().getBeginIndex() < link.getEndIndex())) {
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(),
                beginIndex, refTag.getMatchingTag().getBeginIndex());
            String replacement =
                (link.hasSquare() ? "" : "[") +
                contents.substring(beginIndex, refTag.getMatchingTag().getBeginIndex()) +
                "]";
            errorResult.addReplacement(replacement);
            errors.add(errorResult);
          } else {
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(),
                beginIndex, link.getEndIndex());
            errorResult.addReplacement(
                "[" + link.getLink() + " " + link.getText().replaceAll("\\n", "") + "]");
            errors.add(errorResult);
          }
        }
      }
    }
    return result;
  }
}
