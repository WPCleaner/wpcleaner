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
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.Page;

/**
 * Algorithm for analyzing error 51 of check wikipedia project.
 * Error 51: Interwiki before last headline.
 */
public class CheckErrorAlgorithm051 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm051() {
    super("Interwiki before last headline");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, Collection<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Searching for last headline
    int startIndex = contents.length();
    int lastHeadline = -1;
    while ((startIndex >= 0) && (lastHeadline < 0)) {
      int lineIndex = contents.lastIndexOf("=", startIndex);
      if (lineIndex > 0) {
        if ((lineIndex == 0) || (contents.charAt(lineIndex - 1) == '\n')) {
          lastHeadline = lineIndex;
        }
      }
      startIndex = lineIndex - 1;
    }
    if (lastHeadline < 0) {
      return false;
    }

    startIndex = 0;
    boolean result = false;
    while (startIndex < lastHeadline) {

      // Searching for next [[
      int beginIndex = contents.indexOf("[[", startIndex);
      if ((beginIndex < 0) || (beginIndex > lastHeadline)) {
        startIndex = contents.length();
      } else {
        int colonIndex = contents.indexOf(":", beginIndex);
        if (colonIndex >= 0) {
          int endIndex = contents.indexOf("]]", beginIndex);
          if (endIndex > colonIndex) {
            String namespace = contents.substring(beginIndex + 2, colonIndex).trim();
            for (Language lg : page.getWikipedia().getLanguages()) {
              if (namespace.equals(lg.getCode())) {
                if (errors == null) {
                  return true;
                }
                result = true;
                CheckErrorResult errorResult = createCheckErrorResult(
                    page, beginIndex, endIndex + 2);
                errors.add(errorResult);
              }
            }
          }
        }
        startIndex = beginIndex + 2;
      }
    }
    return result;
  }
}
