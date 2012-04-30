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

package org.wikipediacleaner.api.data;


/**
 * Utility class to manage page contents.
 */
public class PageContents {

  // ==========================================================================
  // General methods
  // ==========================================================================

  /**
   * Find the first occurrence of a character in a substring.
   * 
   * @param text String.
   * @param character Character.
   * @param begin Beginning of the substring.
   * @param end End of the substring.
   * @return First occurrence of character.
   */
  public static int findCharacter(
      String text, char character, int begin, int end) {
    if (text == null) {
      return -1;
    }
    for (int i = begin; i < end; i++) {
      if (text.charAt(i) == character) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Expand text for a page (for example, replacing {{PAGENAME}}).
   * 
   * @param page Page.
   * @param text Text to expand.
   * @return Expanded text.
   */
  public static String expandText(Page page, String text) {
    if ((page == null) || (text == null)) {
      return text;
    }
    String result = text;
    result = result.replaceAll("\\{\\{PAGENAME\\}\\}", page.getValuePAGENAME());
    return result;
  }

  // ==========================================================================
  // Tag management
  // ==========================================================================

  /**
   * Find the first tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tagName Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTagFull findNextTagFull(
      Page page, String contents,
      String tagName, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while (currentIndex < contents.length()) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementTagFull tag = PageElementTagFull.analyzeBlock(
            tagName, contents, tmpIndex);
        if (tag != null) {
          return tag;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }
}
