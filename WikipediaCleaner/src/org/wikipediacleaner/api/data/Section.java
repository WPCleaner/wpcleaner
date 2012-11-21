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

package org.wikipediacleaner.api.data;


/**
 * Bean for holding information a section.
 */
public class Section {

  /**
   * Title level in the table of contents.
   */
  private final int tocLevel;

  /**
   * Title level.
   */
  private final int level;

  /**
   * Title.
   */
  private final String line;

  /**
   * Title number.
   */
  private final String number;

  /**
   * Title index.
   */
  private final int index;

  public Section(
      int tocLevel, int level,
      String line, String number, int index) {
    this.tocLevel = tocLevel;
    this.level = level;
    this.line = line;
    this.number = number;
    this.index = index;
  }

  /**
   * @return Title level in the table of contents.
   */
  public int getTocLevel() {
    return tocLevel;
  }

  /**
   * @return Title level.
   */
  public int getLevel() {
    return level;
  }

  /**
   * @return Title.
   */
  public String getLine() {
    return line;
  }

  /**
   * @return Title number.
   */
  public String getNumber() {
    return number;
  }

  /**
   * @return Title index.
   */
  public int getIndex() {
    return index;
  }
}
