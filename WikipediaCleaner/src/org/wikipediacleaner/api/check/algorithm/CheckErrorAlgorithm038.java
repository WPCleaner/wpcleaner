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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 38 of check wikipedia project.
 * Error 38: HTML text style element &lt;i&gt;
 */
public class CheckErrorAlgorithm038 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm038() {
    super("HTML text style element <i>");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param comments Comments in the page contents.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      Page page, String contents,
      Collection<PageElementComment> comments,
      Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      PageElementTag tag = PageContents.findNextTag(page, contents, "i", startIndex);
      if (tag == null) {
        startIndex = contents.length();
      } else {
        if (errors == null) {
          return true;
        }
        CheckErrorResult error = createCheckErrorResult(
            page, tag.getStartTagBeginIndex(), tag.getEndTagEndIndex());
        String text = tag.getText();
        if (text.length() > 30) {
          text = text.substring(0, 10) + "…" + text.substring(text.length() - 10); 
        }
        error.addReplacement(
            "''" + tag.getText() + "''",
            GT._("Replace with {0}", "''" + text + "''"));
        error.addReplacement(
            tag.getText(),
            GT._("Replace with {0}", text));
        errors.add(error);
        result = true;
        startIndex += tag.getEndTagEndIndex();
      }
    }
    return result;
  }
}
