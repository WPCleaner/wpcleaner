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
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 77 of check wikipedia project.
 * Error 77: Image description with partial &lt;small&gt;
 */
public class CheckErrorAlgorithm077 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm077() {
    super("Image description with partial <small>");
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


    // Analyzing all images
    boolean result = false;
    List<PageElementImage> images = pageAnalysis.getImages();
    for (PageElementImage image : images) {
      String description = image.getDescription();
      if (description != null) {
        description = description.trim();
        PageAnalysis descAnalysis = pageAnalysis.getPage().getAnalysis(description, false);
        List<PageElementTag> smallTags = descAnalysis.getTags(PageElementTag.TAG_HTML_SMALL);
        if ((smallTags != null) && (!smallTags.isEmpty())) {
          int lastTest = 0;
          int currentDepth = 0;
          boolean onlySmall = true;
          for (PageElementTag smallTag : smallTags) {
            if ((currentDepth == 0) && (smallTag.getBeginIndex() > lastTest)) {
              onlySmall = false;
            }
            lastTest = smallTag.getEndIndex();
            if (!smallTag.isFullTag()) {
              if (smallTag.isEndTag()) {
                currentDepth = Math.max(0, currentDepth - 1);
              } else {
                currentDepth++;
              }
            }
          }
          if (!onlySmall) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), image.getBeginIndex(), image.getEndIndex());
            StringBuilder replacement = new StringBuilder();
            int lastIndex = 0;
            for (PageElementTag smallTag : smallTags) {
              if (smallTag.getBeginIndex() > lastIndex) {
                replacement.append(description.substring(lastIndex, smallTag.getBeginIndex()));
              }
              lastIndex = smallTag.getEndIndex();
            }
            if (lastIndex < description.length()) {
              replacement.append(description.substring(lastIndex));
            }
            errorResult.addReplacement(
                image.getDescriptionReplacement(replacement.toString()),
                GT._("Remove {0} tags", "<small>"));
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
