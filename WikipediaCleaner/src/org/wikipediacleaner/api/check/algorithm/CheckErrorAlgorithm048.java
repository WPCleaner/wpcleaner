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
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 48 of check wikipedia project.
 * Error 48: Title linked in text
 */
public class CheckErrorAlgorithm048 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Remove all links to title"),
  };

  public CheckErrorAlgorithm048() {
    super("Title linked in text");
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
      // Looking for [[
      startIndex = contents.indexOf("[[", startIndex);
      if (startIndex >= 0) {
        int linkIndex = startIndex + 2;
        // Removing possible whitespaces before link
        while ((linkIndex < contents.length()) && (contents.charAt(linkIndex) == ' ')) {
          linkIndex++;
        }
        if (contents.startsWith(page.getTitle(), linkIndex)) {
          String text = contents.substring(linkIndex, linkIndex + page.getTitle().length());
          linkIndex += page.getTitle().length();
          // Removing possible whitespaces after link
          while ((linkIndex < contents.length()) && (contents.charAt(linkIndex) == ' ')) {
            linkIndex++;
          }
          int endIndex = contents.indexOf("]]", linkIndex);
          if (endIndex < 0) {
            startIndex = contents.length();
          } else {
            if (contents.charAt(linkIndex) == '|') {
              linkIndex++;
              text = contents.substring(linkIndex, endIndex).trim();
              linkIndex = endIndex;
            }
            if (linkIndex == endIndex) {
              if (errors == null) {
                return true;
              }
              result = true;
              CheckErrorResult errorResult = createCheckErrorResult(page, startIndex, endIndex + 2);
              errorResult.addReplacement(text);
              errorResult.addReplacement("'''" + text + "'''");
              errors.add(errorResult);
              startIndex = endIndex + 2;
            } else {
              startIndex = linkIndex;
            }
          }
        } else {
          startIndex = linkIndex;
        }
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, Page page, String contents) {
    return fixUsingFirstReplacement(fixName, page, contents);
  }
}
