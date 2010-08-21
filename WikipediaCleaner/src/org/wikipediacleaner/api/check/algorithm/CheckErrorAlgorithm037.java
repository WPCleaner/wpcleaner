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

import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageContents;

/**
 * Algorithm for analyzing error 37 of check wikipedia project.
 * Error 37: Title with special letters and no DEFAULTSORT
 */
public class CheckErrorAlgorithm037 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm037() {
    super("Title with special letters and no DEFAULTSORT");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(
      Page page, String contents,
      @SuppressWarnings("unused") List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing title to find special characters
    String title = page.getTitle();
    boolean characterFound = false;
    String unknownCharacters = "";
    String text = "";
    int currentPos = 0;
    while (currentPos < title.length()) {
      boolean error = false;
      char character = title.charAt(currentPos);
      if (!SpecialCharacters.isAuthorized(character, page.getWikipedia())) {
        if (currentPos < 3) { // TODO : Parameter
          characterFound = true;
        }
        error = true;
      }
      if (error) {
        String newCharacter = SpecialCharacters.proposeReplacement(character);
        if (!Character.toString(character).equals(newCharacter)) {
          //
        } else {
          unknownCharacters += character;
        }
        text += newCharacter;
      } else {
        text += character;
      }
      currentPos++;
    }
    if (!characterFound) {
      return false;
    }

    // Searching a DEFAULTSORT tag
    PageElementDefaultsort tag = PageContents.findNextDefaultsort(page, contents, 0);
    if (tag != null) {
      return false;
    }

    // Searching for Categories without a sort key
    // TODO

    // Reporting error
    return true;
  }
}
