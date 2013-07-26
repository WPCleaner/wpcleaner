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
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 34 of check wikipedia project.
 * Error 34: Template programming element
 */
public class CheckErrorAlgorithm034 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm034() {
    super("Template programming element");
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
    if (pageAnalysis.isInNamespace(Namespace.TEMPLATE)) {
      return false;
    }

    // Check every position
    Page page = pageAnalysis.getPage();
    String contents = pageAnalysis.getContents();
    int maxLen = contents.length();
    boolean result = false;
    int currentIndex = 0;
    while (currentIndex < maxLen) {
      int nextIndex = currentIndex;
      if (contents.startsWith("{{", currentIndex)) {
        boolean done = false;

        // Check for templates beginning with '{{{' instead of '{{'
        if (!done &&
            contents.startsWith("{{{", currentIndex)) {
          PageElementTemplate currentTemplate = pageAnalysis.isInTemplate(currentIndex);
          PageElementTemplate nextTemplate = pageAnalysis.isInTemplate(currentIndex + 1);
          if ((nextTemplate != null) &&
              (currentIndex + 1 == nextTemplate.getBeginIndex()) &&
              ((currentTemplate == null) ||
               (currentTemplate.getBeginIndex() < currentIndex - 1))) {
            result = true;
            done = true;
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                page, currentIndex, currentIndex + 3);
            errorResult.addReplacement("{{");
            errors.add(errorResult);
            nextIndex = currentIndex + 3;
          }
        }

        // Check for parameters
        if (!done) {
          PageElementParameter parameter = pageAnalysis.isInParameter(currentIndex);
          if ((parameter != null) &&
              (parameter.getBeginIndex() == currentIndex)) {
            result = true;
            done = true;
            if (errors == null) {
              return true;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                page, parameter.getBeginIndex(), parameter.getEndIndex());
            errors.add(errorResult);
            nextIndex = parameter.getEndIndex();
          }
        }

        // Check for functions
        if (!done) {
          PageElementFunction function = pageAnalysis.isInFunction(currentIndex);
          if ((function != null) &&
              (function.getBeginIndex() == currentIndex)) {
            MagicWord magicWord = function.getMagicWord();
            String magicWordName = magicWord.getName();
            boolean isOk = false;
            if (MagicWord.DEFAULT_SORT.equals(magicWordName) ||
                MagicWord.FORMAT_NUM.equals(magicWordName) ||
                MagicWord.DISPLAY_TITLE.equals(magicWordName)) {
              isOk = true;
            }
            if (!isOk &&
                MagicWord.TAG.equals(magicWordName) &&
                (function.getParameterCount() > 0) &&
                (PageElementTag.TAG_WIKI_REF.equals(function.getParameterValue(0)))) {
              isOk = true;
            }
            if (!isOk) {
              result = true;
              done = true;
              if (errors == null) {
                return true;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, function.getBeginIndex(), function.getEndIndex());
              if (MagicWord.PAGE_NAME.equals(magicWordName)) {
                errorResult.addReplacement(page.getTitle());
              }
              errors.add(errorResult);
              nextIndex = function.getEndIndex();
            } else {
              nextIndex = currentIndex + 2;
            }
          }
        }
      }
      currentIndex = Math.max(nextIndex, currentIndex  + 1);
    }

    return result;
  }
}
