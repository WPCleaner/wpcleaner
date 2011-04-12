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
import org.wikipediacleaner.api.data.PageElementComment;


/**
 * Algorithm for analyzing error 85 of check wikipedia project.
 * Error 85: Tag without content
 */
public class CheckErrorAlgorithm085 extends CheckErrorAlgorithmBase {

  private final static String[] tags = {
    "includeonly",
    "noinclude",
  };

  public CheckErrorAlgorithm085() {
    super("Tag without content");
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

    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      // Looking for <
      startIndex = contents.indexOf("<", startIndex);
      if (startIndex < 0) {
        startIndex = contents.length();
      } else {

        // Check if a tag begins
        String tag = null;
        int currentPos = startIndex;
        for (int i = 0; (i < tags.length) && (tag == null); i++) {
          if ((contents.charAt(startIndex) == '<') &&
              (contents.startsWith(tags[i], startIndex + 1)) &&
              (contents.charAt(startIndex + tags[i].length() + 1) == '>')) {
            tag = tags[i];
            currentPos += tag.length() + 2;
          }
        }

        if (tag == null) {
          startIndex++;
        } else {

          // Possible whitespaces
          while ((currentPos < contents.length()) &&
                 (Character.isWhitespace(contents.charAt(currentPos)))) {
            currentPos++;
          }

          // Check if the tag ends
          if ((currentPos + tag.length() + 2 < contents.length()) &&
              (contents.charAt(currentPos) == '<') &&
              (contents.charAt(currentPos + 1) == '/') &&
              (contents.startsWith(tag, currentPos + 2)) &&
              (contents.charAt(currentPos + tag.length() + 2) == '>')) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                page, startIndex, currentPos + tag.length() + 3);
            errors.add(errorResult);
            startIndex = currentPos + tag.length() + 3;
          } else {
            startIndex++;
          }
        }
      }
    }
    return result;
  }
}
