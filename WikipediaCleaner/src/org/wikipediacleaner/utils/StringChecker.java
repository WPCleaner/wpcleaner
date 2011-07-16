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


/**
 * A basic interface for checking that a String follow some rules.
 */
public interface StringChecker {

  /**
   * Holder for result.
   */
  public static class Result {

    /**
     * Is the result OK ?
     */
    private final boolean ok;

    /**
     * What is the text result ?
     */
    private final String text;

    /**
     * Explanation if the result is not OK.
     */
    private final String message;

    /**
     * @param ok Is the result OK ?
     * @param text Text result.
     * @param message Explanation.
     */
    public Result(boolean ok, String text, String message) {
      this.ok = ok;
      this.text = text;
      this.message = message;
    }

    /**
     * @return Is the result OK ?
     */
    public boolean isOk() {
      return ok;
    }

    /**
     * @return Text.
     */
    public String getText() {
      return text;
    }

    /**
     * @return Explanation.
     */
    public String getMessage() {
      return message;
    }
  }

  /**
   * Check if a text follows the rules.
   * 
   * @param text Text to check.
   * @return Result.
   */
  public Result checkString(String text);
}
