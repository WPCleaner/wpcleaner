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
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageAnalysis;


/**
 * Algorithm for analyzing error 91 of check wikipedia project.
 * Error 91: DEFAULTSORT is missing and title with lowercase_letters
 */
public class CheckErrorAlgorithm091 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm091() {
    super("DEFAULTSORT is missing and title with lowercase_letters");
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

    // Checking if title contains lowercase letters
    boolean lowFound = false;
    boolean firstLetter = true;
    String title = pageAnalysis.getPage().getTitle();
    for (int currentPos = 0; currentPos < title.length(); currentPos++) {
      if (Character.isLowerCase(title.charAt(currentPos))) {
        if (firstLetter) {
          lowFound = true;
        }
        firstLetter = false;
      } else if (Character.isWhitespace(title.charAt(currentPos))) {
        firstLetter = true;
      } else {
        firstLetter = false;
      }
    }
    if (!lowFound) {
      return false;
    }

    // Analyzing the text from the beginning
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      // Update position of next {{
      int beginIndex = contents.indexOf("{{", startIndex);

      if (beginIndex < 0) {
        // No more {{
        startIndex = contents.length();
      } else {
        int currentPos = beginIndex + 2;

        // Update position of next }}
        int endIndex = contents.indexOf("}}", currentPos);

        if (endIndex < 0) {
          startIndex = contents.length();
        } else {
          // Possible whitespaces
          while ((currentPos < endIndex) && Character.isWhitespace(contents.charAt(currentPos))) {
            currentPos++;
          }

          // Check that link is DEFAULTSORT
          String defaultSort = null;
          if (currentPos < endIndex) {
            MagicWord magicDefaultsort = pageAnalysis.getWikipedia().getMagicWord(
                MagicWord.DEFAULT_SORT);
            List<String> aliases = magicDefaultsort.getAliases();
            for (int i = 0; (i < aliases.size()) && (defaultSort == null); i++) {
              if (contents.startsWith(aliases.get(i), currentPos)) {
                currentPos += aliases.get(i).length();
                defaultSort = aliases.get(i);
              }
            }
          }

          // DEFAULTSORT found
          if ((currentPos < endIndex) && (defaultSort != null)) {
            return false;
          }
          startIndex = endIndex + 2;
        }
      }
    }
    return true;
  }
}
