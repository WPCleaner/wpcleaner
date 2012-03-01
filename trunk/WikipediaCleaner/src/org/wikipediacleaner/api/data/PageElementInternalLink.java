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
 * Class containing information about a complete internal link ([[link#anchor|text]]). 
 */
public class PageElementInternalLink extends PageElement {

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
  public static PageElementInternalLink analyzeBlock(
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

    // Find elements of internal link
    int endIndex = -1;
    int anchorIndex = -1;
    int pipeIndex = -1;
    int levelSquareBrackets = 0;
    int levelCurlyBrackets = 0;
    while ((tmpIndex < contents.length()) && (endIndex < 0)) {
      switch (contents.charAt(tmpIndex)) {
      case ']':
        if (contents.startsWith("]]", tmpIndex)) {
          if (levelSquareBrackets > 0) {
            levelSquareBrackets--;
          } else {
            endIndex = tmpIndex;
          }
          tmpIndex++;
        }
        break;
      case '[':
        if (contents.startsWith("[[", tmpIndex)) {
          levelSquareBrackets++;
          tmpIndex++;
        }
        break;
      case '{':
        if (contents.startsWith("{{", tmpIndex)) {
          levelCurlyBrackets++;
          tmpIndex++;
        }
        break;
      case '}':
        if (contents.startsWith("}}", tmpIndex)) {
          if (levelCurlyBrackets > 0) {
            levelCurlyBrackets--;
          }
          tmpIndex++;
        }
        break;
      case '#':
        if ((levelSquareBrackets == 0) &&
            (levelCurlyBrackets == 0) &&
            (anchorIndex < 0)) {
          anchorIndex = tmpIndex;
        }
        break;
      case '|':
        if ((levelSquareBrackets == 0) &&
            (levelCurlyBrackets == 0) &&
            (pipeIndex < 0)) {
          pipeIndex = tmpIndex;
        }
      }
      tmpIndex++;
    }
    if (endIndex < 0) {
      return null;
    }

    // Extract link elements
    String link = null;
    String anchor = null;
    String text = null;
    if ((pipeIndex >= 0) && (pipeIndex < endIndex)) {
      if ((anchorIndex >= 0) && (anchorIndex < pipeIndex)) {
        link = contents.substring(beginIndex, anchorIndex);
        anchor = contents.substring(anchorIndex + 1, pipeIndex);
        text = contents.substring(pipeIndex + 1, endIndex);
      } else {
        link = contents.substring(beginIndex, pipeIndex);
        text = contents.substring(pipeIndex + 1, endIndex);
      }
    } else if ((anchorIndex >= 0) && (anchorIndex < endIndex)) {
      link = contents.substring(beginIndex, anchorIndex);
      anchor = contents.substring(anchorIndex + 1, endIndex);
    } else {
      link = contents.substring(beginIndex, endIndex);
    }

    // Check that it is really an internal link
    String linkTrimmed = link.trim();
    int colonIndex = linkTrimmed.indexOf(':');
    if (colonIndex > 0) {
      String namespaceName = linkTrimmed.substring(0, colonIndex);

      // Is it a category ?
      Namespace category = Namespace.getNamespace(Namespace.CATEGORY, wikipedia.getNamespaces());
      if ((category != null) && (category.isPossibleName(namespaceName))) {
        return null;
      }

      // Is it a file / image ?
      Namespace image = Namespace.getNamespace(Namespace.IMAGE, wikipedia.getNamespaces());
      if ((image != null) && (image.isPossibleName(namespaceName))) {
        return null;
      }

      // Is it an interwiki ?
      for (Interwiki iw : wikipedia.getInterwikis()) {
        if (iw.getPrefix().equals(namespaceName)) {
          return null;
        }
      }

      // Is it a language link ?
      if (Language.isLanguageCode(wikipedia.getLanguages(), namespaceName)) {
        return null;
      }
    }

    // Create internal link
    return new PageElementInternalLink(
        wikipedia,
        index, endIndex + 2,
        link, anchor, text);
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

  public String getDisplayedText() {
    if (text != null) {
      return text;
    }
    if (anchor == null) {
      return linkNotTrimmed;
    }
    return linkNotTrimmed + "#" + anchorNotTrimmed;
  }

  private PageElementInternalLink(
      EnumWikipedia wikipedia,
      int beginIndex, int endIndex,
      String link, String anchor, String text) {
    super(beginIndex, endIndex);
    this.linkNotTrimmed = link;
    this.link = (link != null) ? wikipedia.normalizeTitle(link.trim()) : null;
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
