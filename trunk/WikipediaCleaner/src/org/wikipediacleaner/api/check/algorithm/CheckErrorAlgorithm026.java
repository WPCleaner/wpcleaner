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
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 26 of check wikipedia project.
 * Error 26: HTML text style element &lt;b&gt;
 */
public class CheckErrorAlgorithm026 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm026() {
    super("HTML text style element <b>");
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

    // Retrieve all <b> tags
    List<PageElementTag> bTags = pageAnalysis.getTags(PageElementTag.TAG_HTML_B);
    boolean result = false;
    for (PageElementTag bTag : bTags) {

      // Check if the tag is an error
      boolean errorFound = false;
      if (bTag.isFullTag()) {
        errorFound = true;
      } else if (bTag.isEndTag()) {
        if (!bTag.isComplete()) {
          errorFound = true;
        }
      } else {
        errorFound = true;
      }

      // Mark error
      if (errorFound) {
        if (errors == null) {
          return true;
        }
        result = true;
        if (!bTag.isFullTag() && bTag.isComplete()) {
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(),
              bTag.getCompleteBeginIndex(),
              bTag.getCompleteEndIndex());
          String text = pageAnalysis.getContents().substring(
              bTag.getValueBeginIndex(),
              bTag.getValueEndIndex());
          if ((text != null) && (text.trim().length() > 0)) {
            String visibleText = text;
            if (text.length() > 30) {
              visibleText = text.substring(0, 10) + "…" + text.substring(text.length() - 10); 
            }
            error.addReplacement(
                "'''" + text + "'''",
                GT._("Replace with {0}", "'''" + visibleText + "'''"));
            error.addReplacement(
                text,
                GT._("Replace with {0}", visibleText));
          } else {
            error.addReplacement("", GT._("Delete"));
          }
          errors.add(error);
        } else {
          CheckErrorResult error = createCheckErrorResult(
              pageAnalysis.getPage(), bTag.getBeginIndex(), bTag.getEndIndex());
          error.addReplacement("", GT._("Delete"));
          errors.add(error);
        }
      }
    }
    return result;
  }
}
