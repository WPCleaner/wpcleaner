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

import java.util.ArrayList;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TemplateBlock;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 59 of check wikipedia project.
 * Error 59: Template value end with break
 */
public class CheckErrorAlgorithm059 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm059() {
    super("Template value end with break");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#isFullDetection()
   */
  @Override
  public boolean isFullDetection() {
    return false;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing from the begining
    boolean errorFound = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      TemplateBlock template = findNextTemplate(page, contents, startIndex);
      if (template == null) {
        startIndex = contents.length();
      } else {
        for (int i = 0; i < template.getParameterCount(); i++) {
          String parameterValue = template.getParameterValue(i);
          if (parameterValue != null) {
            boolean ok = true;
            int tmpIndex = parameterValue.length() - 1;
            while ((tmpIndex >= 0) && (parameterValue.charAt(tmpIndex) == ' ')) {
              tmpIndex--;
            }
            int endIndex = tmpIndex;
            if ((tmpIndex >= 0) && (parameterValue.charAt(tmpIndex) == '>')) {
              tmpIndex--;
            } else {
              ok = false;
            }
            if ((tmpIndex >= 0) && (parameterValue.charAt(tmpIndex) == '/')) {
              tmpIndex--;
            } else {
              ok = false;
            }
            while ((tmpIndex >= 0) && (parameterValue.charAt(tmpIndex) == ' ')) {
              tmpIndex--;
            }
            if ((tmpIndex >= 1) && (parameterValue.startsWith("br", tmpIndex - 1))) {
              tmpIndex -= 2;
            } else {
              ok = false;
            }
            while ((tmpIndex >= 0) && (parameterValue.charAt(tmpIndex) == ' ')) {
              tmpIndex--;
            }
            if ((tmpIndex >= 0) && (parameterValue.charAt(tmpIndex) == '<')) {
              tmpIndex--;
            } else {
              ok = false;
            }
            if (ok) {
              if (errors == null) {
                return true;
              }
              errorFound = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  page,
                  template.getParameterValueOffset(i) + tmpIndex + 1,
                  template.getParameterValueOffset(i) + endIndex + 1);
              errorResult.addReplacement("", GT._("Delete"));
              errors.add(errorResult);
            }
          }
        }
        startIndex = template.getBeginIndex() + 2;
      }
    }

    // Result
    return errorFound;
  }
}
