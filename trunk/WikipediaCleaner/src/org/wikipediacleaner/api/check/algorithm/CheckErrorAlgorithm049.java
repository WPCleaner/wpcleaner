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
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;


/**
 * Algorithm for analyzing error 49 of check wikipedia project.
 * Error 49: Headline with HTML
 */
public class CheckErrorAlgorithm049 extends CheckErrorAlgorithmBase {

  /**
   * Tags that can be detected.
   */
  private static final String[] titleTags = {
    PageElementTag.TAG_HTML_H1,
    PageElementTag.TAG_HTML_H2,
    PageElementTag.TAG_HTML_H3,
    PageElementTag.TAG_HTML_H4,
    PageElementTag.TAG_HTML_H5,
    PageElementTag.TAG_HTML_H6,
    PageElementTag.TAG_HTML_H7,
    PageElementTag.TAG_HTML_H8,
    PageElementTag.TAG_HTML_H9
  };

  public CheckErrorAlgorithm049() {
    super("Headline with HTML");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    if (analysis == null) {
      return false;
    }

    // Analyzing each possible tag
    boolean result = false;
    for (int level = 0; level < titleTags.length; level++) {
      String tagName = titleTags[level];
      List<PageElementTag> tags = analysis.getCompleteTags(tagName);
      if (tags != null) {
        for (PageElementTag tag : tags) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Find possible replacement
          String replacement = analysis.getContents().substring(
              tag.getValueBeginIndex(), tag.getValueEndIndex());

          // Create error
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis.getPage(), tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
          errorResult.addReplacement(PageElementTitle.createTitle(level + 1, replacement));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }
}
