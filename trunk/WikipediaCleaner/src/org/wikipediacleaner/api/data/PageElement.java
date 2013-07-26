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

package org.wikipediacleaner.api.data;


/**
 * Class containing information about an element of the page.
 */
public abstract class PageElement {

  private final int beginIndex;
  private final int endIndex;

  /**
   * @param beginIndex Beginning of the page element.
   * @param endIndex End of the page element.
   */
  public PageElement(int beginIndex, int endIndex) {
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
  }

  /**
   * @return Beginning of the page element.
   */
  public int getBeginIndex() {
    return beginIndex;
  }

  /**
   * @return End of the page element.
   */
  public int getEndIndex() {
    return endIndex;
  }
}
