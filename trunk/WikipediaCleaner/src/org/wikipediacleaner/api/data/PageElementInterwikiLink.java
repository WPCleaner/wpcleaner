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
 * Class containing information about a complete interwiki link ([[iw:link#anchor|text]]). 
 */
public class PageElementInterwikiLink extends PageElement {

  private final String interwikiNotTrimmed;
  private final String interwiki;
  private final String linkNotTrimmed;
  private final String link;
  private final String anchorNotTrimmed;
  private final String anchor;
  private final String textNotTrimmed;
  private final String text;

  /**
   * Analyze contents to check if it matches an internal link.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementInterwikiLink analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '[['
    int tmpIndex = index;
    if ((tmpIndex >= contents.length()) ||
        (!contents.startsWith("[[", tmpIndex))) {
      return null;
    }
    tmpIndex += 2;
    int beginIndex = tmpIndex;

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Find elements of interwiki link
    if (tmpIndex >= contents.length()) {
      return null;
    }
    int endIndex = contents.indexOf("]]", tmpIndex);
    if (endIndex < 0) {
      return null;
    }
    int colonIndex = PageContents.findCharacter(contents, ':', tmpIndex, endIndex);
    if (colonIndex < 0) {
      return null;
    }
    String interwikiText = contents.substring(beginIndex, colonIndex);
    List<Interwiki> interwikis = wikipedia.getInterwikis();
    if (interwikis == null) {
      return null;
    }
    boolean interwikiFound = false;
    for (Interwiki interwiki : interwikis) {
      if ((interwiki != null) &&
          ((interwiki.getLanguage() == null) || (interwiki.getLanguage().length() == 0)) &&
          (interwikiText.equals(interwiki.getPrefix()))) {
        interwikiFound = true;
      }
    }
    if (!interwikiFound) {
      return null;
    }

    int anchorIndex = PageContents.findCharacter(contents, '#', tmpIndex, endIndex);
    int pipeIndex = PageContents.findCharacter(contents, '|', tmpIndex, endIndex);

    // Create interwiki link
    if ((pipeIndex >= 0) && (pipeIndex < endIndex)) {
      if ((anchorIndex >= 0) && (anchorIndex < pipeIndex)) {
        return new PageElementInterwikiLink(
            index, endIndex + 2,
            interwikiText,
            contents.substring(colonIndex + 1, anchorIndex),
            contents.substring(anchorIndex + 1, pipeIndex),
            contents.substring(pipeIndex + 1, endIndex));
      }
      return new PageElementInterwikiLink(
          index, endIndex + 2,
          interwikiText,
          contents.substring(colonIndex + 1, pipeIndex),
          null,
          contents.substring(pipeIndex + 1, endIndex));
    }
    if ((anchorIndex >= 0) && (anchorIndex < endIndex)) {
      return new PageElementInterwikiLink(
          index, endIndex + 2,
          interwikiText,
          contents.substring(colonIndex + 1, anchorIndex),
          contents.substring(anchorIndex + 1, endIndex), null);
    }
    return new PageElementInterwikiLink(
        index, endIndex + 2,
        interwikiText,
        contents.substring(colonIndex + 1, endIndex),
        null, null);
  }

  public String getInterwiki() {
    return interwiki;
  }

  public String getLink() {
    return link;
  }

  public String getAnchor() {
    return anchor;
  }

  public String getFullLink() {
    if (anchor == null) {
      return link;
    }
    return link + "#" + anchor;
  }

  public String getText() {
    return text;
  }

  private PageElementInterwikiLink(
      int beginIndex, int endIndex,
      String interwiki,
      String link, String anchor, String text) {
    super(beginIndex, endIndex);
    this.interwikiNotTrimmed = interwiki;
    this.interwiki = (interwiki != null) ? interwiki.trim() : null;
    this.linkNotTrimmed = link;
    this.link = (link != null) ? Page.getStringUcFirst(link.trim()) : null;
    this.anchorNotTrimmed = anchor;
    this.anchor = (anchor != null) ? anchor.trim() : null;
    this.textNotTrimmed = text;
    this.text = (text != null) ? text.trim() : null;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    sb.append(interwikiNotTrimmed);
    sb.append(':');
    sb.append(linkNotTrimmed);
    if (anchorNotTrimmed != null) {
      sb.append('#');
      sb.append(anchorNotTrimmed);
    }
    if (textNotTrimmed != null) {
      sb.append('|');
      sb.append(textNotTrimmed);
    }
    sb.append("]]");
    return sb.toString();
  }
}
