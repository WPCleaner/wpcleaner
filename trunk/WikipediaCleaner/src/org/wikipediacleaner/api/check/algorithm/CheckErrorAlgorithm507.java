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
import org.wikipediacleaner.api.data.PageElementTag.Parameter;


/**
 * Algorithm for analyzing error 507 of check wikipedia project.
 * Error 507: Gallery without caption
 */
public class CheckErrorAlgorithm507 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm507() {
    super("Gallery without caption");
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

    // Analyze each gallery tag
    List<PageElementTag> galleryTags = pageAnalysis.getTags(PageElementTag.TAG_WIKI_GALLERY);
    boolean result = false;
    for (PageElementTag galleryTag : galleryTags) {
      if (!galleryTag.isFullTag() && !galleryTag.isEndTag()) {
        Parameter description = galleryTag.getParameter("caption");
        if ((description == null) ||
            (description.getTrimmedValue() == null) ||
            (description.getTrimmedValue().length() == 0)) {
          if (errors == null) {
            return true;
          }
          result = true;

          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), galleryTag.getBeginIndex(), galleryTag.getEndIndex());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
