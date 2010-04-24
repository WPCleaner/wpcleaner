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
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 45 of check wikipedia project.
 * Error 45: Interwiki double
 */
public class CheckErrorAlgorithm045 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm045() {
    super("Interwiki double");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    int startIndex = 0;
    ArrayList<String> knownLanguages = new ArrayList<String>();
    while (startIndex < contents.length()) {
      // Update position of next [[
      int beginIndex = contents.indexOf("[[", startIndex);

      if (beginIndex < 0) {
        // No more [[
        startIndex = contents.length();
      } else {
        int currentPos = beginIndex + 2;

        // Update position of next ]]
        int endIndex = contents.indexOf("]]", currentPos);

        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          // Possible whitespaces
          while ((currentPos < endIndex) && Character.isWhitespace(contents.charAt(currentPos))) {
            currentPos++;
          }
          int beginLink = currentPos;
          while ((currentPos < endIndex) &&
              (contents.charAt(currentPos) != ':') &&
              (contents.charAt(currentPos) != '|')) {
            currentPos++;
          }
          // Check that link starts with :
          if ((currentPos < endIndex) && (contents.charAt(currentPos) == ':')) {
            String namespace = contents.substring(beginLink, currentPos);
            for (Language lg : page.getWikipedia().getLanguages()) {
              if (namespace.equals(lg.getCode())) {
                if (!knownLanguages.contains(lg.getCode())) {
                  knownLanguages.add(lg.getCode());
                } else {
                  if (errors == null) {
                    return true;
                  }
                  result = true;
                  int endError = endIndex + 2;
                  if ((endError + 1 < contents.length()) && (contents.charAt(endError + 1) == '\n')) {
                    endError++;
                  }
                  CheckErrorResult errorResult = new CheckErrorResult(
                      getShortDescription(), beginIndex, endError);
                  errorResult.addReplacement("");
                  errors.add(errorResult);
                }
              }
            }
          }
          startIndex = endIndex + 2;
        }
      }
    }
    return result;
  }
}
