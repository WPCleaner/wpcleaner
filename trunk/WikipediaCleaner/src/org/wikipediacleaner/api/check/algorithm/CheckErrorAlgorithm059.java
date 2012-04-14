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
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 59 of check wikipedia project.
 * Error 59: Template value end with break
 */
public class CheckErrorAlgorithm059 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm059() {
    super("Template value end with break");
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

    // Retrieve list of <br> tags
    List<PageElementTag> brTags = pageAnalysis.getTags(PageElementTag.TAG_BR);
    int brTagsSize = (brTags != null) ? brTags.size() : 0;
    int currentBrTag = 0;
    if ((brTags == null) || (brTagsSize == 0)) {
      return false;
    }

    // Analyzing from the beginning
    boolean errorFound = false;
    String contents = pageAnalysis.getContents();
    for (PageElementTemplate template : pageAnalysis.getTemplates()) {

      // Find the first <br> tag after template begin
      while ((currentBrTag < brTagsSize) &&
             (brTags.get(currentBrTag).getBeginIndex() < template.getBeginIndex())) {
        currentBrTag++;
      }
      if (currentBrTag >= brTagsSize) {
        return errorFound;
      }
 
      // Check if template has <br> tags in it
      if (brTags.get(currentBrTag).getBeginIndex() < template.getEndIndex()) {

        // Check every parameter
        for (int i = 0; i < template.getParameterCount(); i++) {

          String parameterValue = template.getParameterValue(i);
          if (parameterValue != null) {

            // Find the last <br> tag in parameter
            int lastParamIndex = template.getParameterValueOffset(i) + parameterValue.length();
            PageElementTag lastBrTag = null;
            while ((currentBrTag < brTagsSize) &&
                   (brTags.get(currentBrTag).getBeginIndex() < lastParamIndex)) {
              lastBrTag = brTags.get(currentBrTag);
              currentBrTag++;
            }

            // Check if a <br> tag is at the end of the parameter value
            if (lastBrTag != null) {
              int currentIndex = lastBrTag.getEndIndex();
              boolean ok = true;
              while (currentIndex < lastParamIndex) {
                if (Character.isWhitespace(contents.charAt(currentIndex))) {
                  currentIndex++;
                } else {
                  PageElementComment comment = pageAnalysis.isInComment(currentIndex);
                  if (comment != null) {
                    currentIndex = comment.getEndIndex();
                  } else {
                    currentIndex = lastParamIndex;
                    ok = false;
                  }
                }
              }
              if (ok) {
                if (errors == null) {
                  return true;
                }
                errorFound = true;
                CheckErrorResult errorResult = createCheckErrorResult(
                    pageAnalysis.getPage(),
                    lastBrTag.getBeginIndex(),
                    lastBrTag.getEndIndex());
                errorResult.addReplacement("", GT._("Delete"));
                errors.add(errorResult);
              }
            }
          }
        }
      }
    }

    // Result
    return errorFound;
  }
}
