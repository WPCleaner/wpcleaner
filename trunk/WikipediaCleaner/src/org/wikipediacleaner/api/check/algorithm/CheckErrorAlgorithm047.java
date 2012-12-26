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
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 47 of check wikipedia project.
 * Error 47: Template not correct begin
 */
public class CheckErrorAlgorithm047 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm047() {
    super("Template not correct begin");
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

    // Analyze contents from the beginning
    String contents = pageAnalysis.getContents();
    int maxLength = contents.length();
    int currentIndex = contents.indexOf("}}");
    boolean result = false;
    while (currentIndex > 0) {
      boolean shouldCount = true;
      if ((pageAnalysis.isInComment(currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
          (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
          (pageAnalysis.isInCategory(currentIndex) != null) ||
          (pageAnalysis.isInTag(currentIndex) != null)) {
        shouldCount = false;
      }
      if (shouldCount) {
        PageElementTemplate template = pageAnalysis.isInTemplate(currentIndex);
        if ((template != null) && (template.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementFunction function = pageAnalysis.isInFunction(currentIndex);
        if ((function != null) && (function.getEndIndex() == currentIndex + 2)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if ((currentIndex + 2 < maxLength) &&
            (contents.charAt(currentIndex + 2) == '}')) {
          shouldCount = false;
        }
      }
      if (shouldCount) {

        // Check if there is a potential beginning
        int tmpIndex = currentIndex - 1;
        boolean errorReported = false;
        boolean finished = false;
        while (!finished && tmpIndex >= 0) {
          char tmpChar = contents.charAt(tmpIndex);
          if ((tmpChar == '\n') ||
              (tmpChar == ']') ||
              (tmpChar == '}')) {
            finished = true;
          } else if (tmpChar == '{') {
            if ((tmpIndex == 0) || (contents.charAt(tmpIndex - 1) != '{')) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(), tmpIndex, currentIndex + 2);
              errorResult.addReplacement("{" + contents.substring(tmpIndex, currentIndex + 2));
              errors.add(errorResult);
              result = true;
            }
            errorReported = true;
            finished = true;
          } else if (tmpChar == '[') {
            int firstChar = tmpIndex;
            if ((firstChar > 0) && (contents.charAt(firstChar - 1) == '[')) {
              firstChar--;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(), firstChar, currentIndex + 2);
            errorResult.addReplacement("{{" + contents.substring(tmpIndex + 1, currentIndex + 2));
            errorResult.addReplacement("[[" + contents.substring(tmpIndex + 1, currentIndex) + "]]");
            errors.add(errorResult);
            result = true;
            errorReported = true;
            finished = true;
          }
          tmpIndex--;
        }

        // Default
        if (!errorReported) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), currentIndex, currentIndex + 2);
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
          result = true;
        }
      }
      currentIndex = contents.indexOf("}}", currentIndex + 2);
    }

    return result;
  }
}
