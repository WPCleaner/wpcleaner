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

package org.wikipediacleaner.api.check;

import java.util.ArrayList;

import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 91 of check wikipedia project.
 * Error 91: DEFAULTSORT is missing and title with lowercase_letters
 */
public class CheckErrorAlgorithm91 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm91() {
    super("DEFAULTSORT is missing and title with lowercase_letters");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(
      Page page, String contents,
      @SuppressWarnings("unused") ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Checking if title contains lowercase letters
    boolean lowFound = false;
    boolean firstLetter = true;
    for (int currentPos = 0; currentPos < page.getTitle().length(); currentPos++) {
      if (Character.isLowerCase(page.getTitle().charAt(currentPos))) {
        if (firstLetter) {
          lowFound = true;
        }
        firstLetter = false;
      } else if (Character.isWhitespace(page.getTitle().charAt(currentPos))) {
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
            MagicWord magicDefaultsort = page.getWikipedia().getMagicWord(MagicWord.DEFAULT_SORT);
            ArrayList<String> aliases = magicDefaultsort.getAliases();
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
