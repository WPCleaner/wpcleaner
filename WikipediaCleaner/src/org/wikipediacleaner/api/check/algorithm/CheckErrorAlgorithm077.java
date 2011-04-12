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
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;


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

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    while (startIndex < contents.length()) {
      PageElementImage image = PageContents.findNextImage(page, contents, startIndex);
      if (image != null) {
        startIndex = image.getEndIndex();
        String text = image.getDescription();
        if (text != null) {
          PageElementTag tag = PageContents.findNextTag(page, text, "small", 0);
          if (tag != null) {
            boolean onlySpaces = true;
            int tmpIndex = 0;
            while (tmpIndex < tag.getStartTagBeginIndex()) {
              if (!Character.isWhitespace(text.charAt(tmpIndex))) {
                onlySpaces = false;
              }
              tmpIndex++;
            }
            tmpIndex = tag.getEndTagEndIndex() + 1;
            while (tmpIndex < text.length()) {
              if (!Character.isWhitespace(text.charAt(tmpIndex))) {
                onlySpaces = false;
              }
              tmpIndex++;
            }
            if (!onlySpaces) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(
                  page, image.getBeginIndex(), image.getEndIndex());
              errors.add(errorResult);
            }
          }
        }
      } else {
        startIndex = contents.length();
      }
    }

    return result;
  }
}
