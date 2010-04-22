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

import org.wikipediacleaner.api.data.Page;


/**
 * Algorithm for analyzing error 16 of check wikipedia project.
 * Error 16: Template with Unicode control characters
 */
public class CheckErrorAlgorithm16 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm16() {
    super("Template with Unicode control characters");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.ArrayList)
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    boolean result = false;
    result |= simpleTextSearch(page, contents, errors, Character.toString((char) 0xFEFF), " ");
    result |= simpleTextSearch(page, contents, errors, Character.toString((char) 0x200E), " ");
    result |= simpleTextSearch(page, contents, errors, Character.toString((char) 0x200B), " ");
    return result;
  }
}
