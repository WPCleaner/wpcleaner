/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.api.constants;

import org.wikipediacleaner.api.data.Page;


/**
 * Encapsulate possible rules for Case sensitiveness.
 */
public enum EnumCaseSensitiveness {

  FIRST_LETTER("first-letter"),
  CASE_SENSITIVE("case-sensitive"),
  UNKNOWN("");

  /**
   * Code representing the case sensitiveness.
   */
  private final String code;

  /**
   * @param code Code representing the case sensitiveness.
   */
  private EnumCaseSensitiveness(String code) {
    this.code = code;
  }

  /**
   * Retrieve value matching a given code.
   * 
   * @param code Code.
   * @return Value for the case sensitiveness.
   */
  public static EnumCaseSensitiveness getCase(String code) {
    if (FIRST_LETTER.code.equals(code)) {
      return FIRST_LETTER;
    }
    if (CASE_SENSITIVE.code.equals(code)) {
      return CASE_SENSITIVE;
    }
    return UNKNOWN;
  }

  /**
   * Normalize a text.
   * 
   * @param text Text to be normalized.
   * @return Normalized text.
   */
  public String normalize(String text) {
    if (text == null) {
      return null;
    }
    String result = text.trim();
    result = result.replaceAll("_", " ");
    result = result.replaceAll(" +", " ");
    result = result.trim();
    if ((this == FIRST_LETTER) &&
        (result.length() > 0) &&
        (Character.isLowerCase(result.charAt(0)))) {
      result = Page.getStringUcFirst(result);
    }
    return result;
  }
}
