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

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a complete external link ([http://... text]). 
 */
public class PageElementExternalLink extends PageElement {

  private final String linkNotTrimmed;
  private final String link;
  private final String textNotTrimmed;
  private final String text;

  /**
   * Analyze contents to check if it matches an external link.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementExternalLink analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '['
    int tmpIndex = index;
    if ((tmpIndex >= contents.length()) ||
        (!contents.startsWith("[", tmpIndex))) {
      return null;
    }
    tmpIndex++;
    int beginIndex = tmpIndex;

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Check for protocol
    if (tmpIndex >= contents.length()) {
      return null;
    }
    if ((!contents.startsWith("http://", tmpIndex)) &&
        (!contents.startsWith("https://", tmpIndex)) &&
        (!contents.startsWith("ftp://", tmpIndex))) {
      return null;
    }

    // Find end of external link
    int endIndex = contents.indexOf(']', tmpIndex);
    if (endIndex < 0) {
      return null;
    }

    // Find possible description
    int spaceIndex = contents.indexOf(' ', tmpIndex);
    if ((spaceIndex < 0) || (spaceIndex >= endIndex)) {
      return new PageElementExternalLink(
          index, endIndex + 1,
          contents.substring(beginIndex, endIndex),
          null);
    }

    return new PageElementExternalLink(
        index, endIndex + 1,
        contents.substring(beginIndex, spaceIndex),
        contents.substring(spaceIndex + 1, endIndex));
  }

  public String getLink() {
    return link;
  }

  public String getText() {
    return text;
  }

  public String getDisplayedText() {
    if (text != null) {
      return text;
    }
    return linkNotTrimmed;
  }

  private PageElementExternalLink(
      int beginIndex, int endIndex,
      String link, String text) {
    super(beginIndex, endIndex);
    this.linkNotTrimmed = link;
    this.link = (link != null) ? link.trim() : null;
    this.textNotTrimmed = text;
    this.text = (text != null) ? text.trim() : null;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[");
    sb.append(linkNotTrimmed);
    if (textNotTrimmed != null) {
      sb.append(' ');
      sb.append(textNotTrimmed);
    }
    sb.append("]");
    return sb.toString();
  }
}
