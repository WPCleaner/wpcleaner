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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a complete external link ([http://... text]). 
 */
public class PageElementExternalLink extends PageElement {

  private final String linkNotTrimmed;
  private final String link;
  private final String textNotTrimmed;
  private final String text;
  private final boolean hasSquare;

  private final static String END_CHARACTERS = " \n\t<>";

  private final static List<String> privateProtocols = new ArrayList<String>();
  private final static List<String> publicProtocols = Collections.unmodifiableList(privateProtocols);

  static {
    privateProtocols.add("http://");
    privateProtocols.add("https://");
    privateProtocols.add("ftp://");
  }

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
    int maxLength = contents.length();

    // Look for '['
    int tmpIndex = index;
    if (tmpIndex >= maxLength) {
      return null;
    }
    boolean hasSquare = false;
    if (contents.startsWith("[", tmpIndex)) {
      hasSquare = true;
      tmpIndex++;
    }
    int beginIndex = tmpIndex;

    // Possible whitespaces characters
    if (hasSquare) {
      while ((tmpIndex < maxLength) &&
             (contents.charAt(tmpIndex) == ' ')) {
        tmpIndex++;
      }
    }

    // Check for protocol
    if (tmpIndex >= maxLength) {
      return null;
    }
    boolean protocolOk = false;
    for (String protocol : privateProtocols) {
      if ((!protocolOk) && (contents.startsWith(protocol, tmpIndex))) {
        protocolOk = true;
      }
    }
    if (!protocolOk) {
      return null;
    }

    // Find end of external link
    int endIndex = -1;
    if (hasSquare) {
      endIndex = contents.indexOf(']', tmpIndex);
    } else {
      endIndex = tmpIndex;
      while ((endIndex < maxLength) &&
             (END_CHARACTERS.indexOf(contents.charAt(endIndex)) < 0)) {
        endIndex++;
      }
      return new PageElementExternalLink(
          index, endIndex,
          contents.substring(beginIndex, endIndex),
          null, hasSquare);
    }
    if (endIndex < 0) {
      return null;
    }

    // Find possible description
    int spaceIndex = contents.indexOf(' ', tmpIndex);
    if ((spaceIndex < 0) || (spaceIndex >= endIndex)) {
      return new PageElementExternalLink(
          index, endIndex + 1,
          contents.substring(beginIndex, endIndex),
          null, hasSquare);
    }

    return new PageElementExternalLink(
        index, endIndex + 1,
        contents.substring(beginIndex, spaceIndex),
        contents.substring(spaceIndex + 1, endIndex),
        hasSquare);
  }

  /**
   * @return List of protocols.
   */
  public static List<String> getProtocols() {
    return publicProtocols;
  }

  /**
   * @return External link.
   */
  public String getLink() {
    return link;
  }

  /**
   * @return Text.
   */
  public String getText() {
    return text;
  }

  /**
   * @return Displayed text.
   */
  public String getDisplayedText() {
    if (text != null) {
      return text;
    }
    return linkNotTrimmed;
  }

  /**
   * @return True if the link is in  [...]
   */
  public boolean hasSquare() {
    return hasSquare;
  }

  private PageElementExternalLink(
      int beginIndex, int endIndex,
      String link, String text,
      boolean hasSquare) {
    super(beginIndex, endIndex);
    this.linkNotTrimmed = link;
    this.link = (link != null) ? link.trim() : null;
    this.textNotTrimmed = text;
    this.text = (text != null) ? text.trim() : null;
    this.hasSquare = hasSquare;
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
