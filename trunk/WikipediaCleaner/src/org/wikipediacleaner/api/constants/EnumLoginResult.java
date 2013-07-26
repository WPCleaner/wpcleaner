/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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


/**
 * Encapsulate possible login results.
 */
public enum EnumLoginResult {

  EMPTY_PASS       ("EmptyPass",       "Empty password"),
  ILLEGAL          ("Illegal",         "Illegal"),
  NEED_TOKEN       ("NeedToken",       "Need token"),
  NEED_TO_WAIT     ("NeedToWait",      "Need to wait"),
  NO_NAME          ("NoName",          "No name"),
  NOT_EXISTS       ("NotExists",       "Not exists"),
  SUCCESS          (null,              "Login successful"),
  WRONG_PASSWORD   ("WrongPass",       "Wrong password"),
  WRONG_PLUGIN_PASS("WrongPluginPass", "Wrong plugin password"),
  
  UNKNOWN_ERROR    (null,              "Unknown error");

  private final String code;
  private final String text;

  /**
   * @param text Associated text
   */
  EnumLoginResult(String code, String text) {
    this.code = code;
    this.text = text;
  }

  /**
   * @param code Error code.
   * @return Matching EnumLoginResult.
   */
  public static EnumLoginResult getEnumByCode(String code) {
    for (EnumLoginResult result : EnumLoginResult.values()) {
      if ((result != null) &&
          (result.code != null) &&
          (result.code.equalsIgnoreCase(code))) {
        return result;
      }
    }
    return UNKNOWN_ERROR;
  }

  /**
   * @return Login successful ?
   */
  public boolean isOk() {
    return equals(SUCCESS);
  }

  /**
   * @return Associated code.
   */
  public String getCode() {
    return code;
  }

  /**
   * @return Associated text.
   */
  public String getText() {
    return text;
  }
}
