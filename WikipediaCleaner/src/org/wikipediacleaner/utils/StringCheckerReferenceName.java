/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.utils;

import org.wikipediacleaner.i18n.GT;


/**
 * Checking that a String can be used to name a reference.
 */
public class StringCheckerReferenceName implements StringChecker {

  /**
   * String containing all unauthorized characters.
   */
  private final static String unauthorized = "[]\"";

  /**
   * Construct a reference name checker
   */
  public StringCheckerReferenceName() {
    //
  }

  /**
   * Check if a text can be used to name a reference.
   * 
   * @param text Text to check.
   * @return Result.
   */
  public Result checkString(String text) {
    if ((unauthorized == null) ||
        (unauthorized.length() == 0) ||
        (text == null) ||
        (text.trim().length() == 0)) {
      return new Result(true, text, null);
    }
    text = text.trim();
    StringBuilder buffer = new StringBuilder(text.length());
    boolean ok = true;
    boolean onlyDigits = true;
    for (int index = 0; index < text.length(); index++) {
      if (unauthorized.indexOf(text.charAt(index)) < 0) {
        buffer.append(text.charAt(index));
        if (!Character.isDigit(text.charAt(index))) {
          onlyDigits = false;
        }
      } else {
        ok = false;
      }
    }
    if (ok && onlyDigits) {
      return new Result(false, buffer.toString(), GT._(
          "A reference name cannot be a numeric key"));
    }
    return new Result(ok, buffer.toString(), GT._(
        "A reference name cannot contain any of these characters: {0}", unauthorized));
  }

}
