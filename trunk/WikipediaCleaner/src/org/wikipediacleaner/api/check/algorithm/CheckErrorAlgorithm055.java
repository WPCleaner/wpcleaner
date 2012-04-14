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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 55 of check wikipedia project.
 * Error 55: HTML text style element &lt;small&gt; double
 */
public class CheckErrorAlgorithm055 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm055() {
    super("HTML text style element <small> double");
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

    // Analyzing the text from the beginning
    Collection<PageElementTag> tags = pageAnalysis.getTags(PageElementTag.TAG_HTML_SMALL);
    if (tags == null) {
      return false;
    }
    int level = 0;
    boolean result = false;
    PageElementTag level0Tag = null;
    for (PageElementTag tag : tags) {
      if (tag.isFullTag()) {
        // Full tag
        if (level == 1) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              tag.getBeginIndex(), tag.getEndIndex());
          errorResult.addReplacement("", GT._("Delete"));
          errors.add(errorResult);
        }
      } else if (tag.isEndTag()) {
        // Closing tag
        level--;
        if (level == 1) {
          if (errors == null) {
            return true;
          }
          result = true;
          if (level0Tag != null) {
            CheckErrorResult errorResult = createCheckErrorResult(
                pageAnalysis.getPage(),
                level0Tag.getBeginIndex(),
                level0Tag.getEndIndex(),
                ErrorLevel.CORRECT);
            errors.add(errorResult);
            if (level0Tag.getMatchingTag() != null) {
              errorResult = createCheckErrorResult(
                  pageAnalysis.getPage(),
                  level0Tag.getMatchingTag().getBeginIndex(),
                  level0Tag.getMatchingTag().getEndIndex(),
                  ErrorLevel.CORRECT);
              errors.add(errorResult);
            }
            level0Tag = null;
          }
          int beginIndex = tag.getBeginIndex();
          if (tag.getMatchingTag() != null) {
            beginIndex = tag.getMatchingTag().getBeginIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(),
              beginIndex, tag.getEndIndex());
          errors.add(errorResult);
        }
        if (level < 0) {
          level = 0;
        }
      } else {
        if (level == 0) {
          level0Tag = tag;
        }
        level++;
      }
    }

    return result;
  }
}
