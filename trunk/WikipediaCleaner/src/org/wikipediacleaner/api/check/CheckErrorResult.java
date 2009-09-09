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


/**
 * A class for memorizing informations about errors detected.
 */
public class CheckErrorResult {

  private final String errorType;
  private final int startPosition;
  private final int endPosition;

  private ArrayList<String> replacements;

  /**
   * Constructor.
   * 
   * @param errorType Type of error.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   */
  public CheckErrorResult(String errorType, int startPosition, int endPosition) {
    this.errorType = errorType;
    this.startPosition = startPosition;
    this.endPosition = endPosition;
    this.replacements = null;
  }

  /**
   * @return Type of error.
   */
  public String getErrorType() {
    return errorType;
  }

  /**
   * @return Start of the error.
   */
  public int getStartPosition() {
    return startPosition;
  }

  /**
   * @return End of the error.
   */
  public int getEndPosition() {
    return endPosition;
  }

  /**
   * @return Length of the error.
   */
  public int getLength() {
    return endPosition - startPosition;
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   */
  public void addReplacement(String replacement) {
    if (replacements == null) {
      replacements = new ArrayList<String>();
    }
    if (!replacements.contains(replacement)) {
      replacements.add(replacement);
    }
  }

  /**
   * @return Possible replacements.
   */
  public ArrayList<String> getReplacements() {
    return replacements;
  }
}
