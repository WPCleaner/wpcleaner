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

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a complete DEFAULTSORT ({{<i>DEFAULTSORT</i>:...}}). 
 */
public class PageElementDefaultsort extends PageElement {

  private final String tag;
  private final String value;

  /**
   * Analyze contents to check if it matches a block for DEFAULTSORT.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementDefaultsort analyzeBlock(EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '{{'
    int beginIndex = index;
    int tmpIndex = beginIndex;
    if ((tmpIndex >= contents.length()) ||
        (!contents.startsWith("{{", tmpIndex))) {
      return null;
    }
    tmpIndex += 2;

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Check that link is DEFAULTSORT
    if (tmpIndex >= contents.length()) {
      return null;
    }
    String defaultSort = null;
    MagicWord magicDefaultsort = wikipedia.getMagicWord(MagicWord.DEFAULT_SORT);
    List<String> aliases = magicDefaultsort.getAliases();
    for (int i = 0; (i < aliases.size()) && (defaultSort == null); i++) {
      if (contents.startsWith(aliases.get(i), tmpIndex)) {
        tmpIndex += aliases.get(i).length();
        defaultSort = aliases.get(i);
      }
    }
    if (defaultSort == null) {
      return null;
    }

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Find end of DEFAULTSORT
    int endIndex = contents.indexOf("}}", tmpIndex);
    if (endIndex < 0) {
      return null;
    }

    return new PageElementDefaultsort(
        beginIndex, endIndex + 2,
        defaultSort, contents.substring(tmpIndex, endIndex).trim());
  }

  public String getTag() {
    return tag;
  }

  public String getValue() {
    return value;
  }

  private PageElementDefaultsort(
      int beginIndex, int endIndex,
      String tag, String value) {
    super(beginIndex, endIndex);
    this.tag = tag;
    this.value = value;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("{{");
    sb.append(tag);
    sb.append(value);
    sb.append("}}");
    return sb.toString();
  }
}
