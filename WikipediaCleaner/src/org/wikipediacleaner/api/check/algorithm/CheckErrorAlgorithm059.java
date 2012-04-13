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

    // Analyzing from the beginning
    boolean errorFound = false;
    for (PageElementTemplate template : pageAnalysis.getTemplates()) {
      for (int i = 0; i < template.getParameterCount(); i++) {
        String parameterValue = template.getParameterValue(i);
        if (parameterValue != null) {

          // Find last <br> tag
          PageAnalysis paramAnalysis = new PageAnalysis(pageAnalysis.getPage(), parameterValue);
          List<PageElementTag> brTags = paramAnalysis.getTags(PageElementTag.TAG_BR);
          if ((brTags != null) && (brTags.size() > 0)) {
            PageElementTag lastBrTag = brTags.get(brTags.size() - 1);
            int currentIndex = lastBrTag.getEndIndex();
            boolean ok = true;
            while (currentIndex < parameterValue.length()) {
              if (Character.isWhitespace(parameterValue.charAt(currentIndex))) {
                currentIndex++;
              } else {
                PageElementComment comment = paramAnalysis.isInComment(currentIndex);
                if (comment != null) {
                  currentIndex = comment.getEndIndex();
                } else {
                  currentIndex = parameterValue.length();
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
                  template.getParameterValueOffset(i) + lastBrTag.getBeginIndex(),
                  template.getParameterValueOffset(i) + lastBrTag.getEndIndex());
              errorResult.addReplacement("", GT._("Delete"));
              errors.add(errorResult);
            }
          }
        }
      }
    }

    // Result
    return errorFound;
  }
}
