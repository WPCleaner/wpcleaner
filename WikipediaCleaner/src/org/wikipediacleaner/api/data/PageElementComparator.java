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

package org.wikipediacleaner.api.data;

import java.util.Comparator;


/**
 * Comparator for PageElement.
 */
public class PageElementComparator implements Comparator<PageElement> {

  public int compare(PageElement o1, PageElement o2) {
    if (o1 == null) {
      if (o2 == null) {
        return 0;
      }
      return -1;
    }
    if (o2 == null) {
      return 1;
    }
    if (o1.getBeginIndex() != o2.getBeginIndex()) {
      return (o1.getBeginIndex() < o2.getBeginIndex()) ? -1 : 1;
    }
    if (o1.getEndIndex() != o2.getEndIndex()) {
      return (o1.getEndIndex() < o2.getEndIndex()) ? -1 : 1;
    }
    return 0;
  }

}
