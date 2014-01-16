/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
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
  private final int    textOffset;
  private final boolean hasSquare;
  private final boolean hasSecondSquare;

  private final static String END_CHARACTERS = " \n\t<>|";

  private final static List<String> privateProtocols = new ArrayList<String>();

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
   * @param analysis Page analysis.
   * @return Block details it there's a block.
   */
  public static PageElementExternalLink analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index,
      PageAnalysis analysis) {
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
    boolean hasSecondSquare = false;
    if (contents.startsWith("[", tmpIndex)) {
      hasSquare = true;
      tmpIndex++;
    }
    int beginIndex = tmpIndex;

    // Possible white spaces characters
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
    boolean protocolOk = isPossibleProtocol(contents, tmpIndex);
    if (!protocolOk) {
      return null;
    }

    // Find end of external link
    int endIndex = -1;
    if (hasSquare) {
      int doubleSquare = 0;
      int tmpIndex2 = tmpIndex;
      int prematureEnd = -1;
      int maxEndIndex = Integer.MAX_VALUE;
      if (analysis != null) {
        PageElementTag refTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, index);
        if ((refTag != null) && !refTag.isFullTag() && refTag.isComplete()) {
          maxEndIndex = refTag.getValueEndIndex();
        }
      }
      while (endIndex < 0) {
        if (tmpIndex2 >= contents.length()) {
          return null;
        }
        if (contents.startsWith("[[", tmpIndex2)) {
          if (prematureEnd < 0) {
            prematureEnd = tmpIndex2;
          }
          doubleSquare++;
          tmpIndex2 += 2;
        } else if ((doubleSquare > 0) && contents.startsWith("]]", tmpIndex2)) {
          doubleSquare--;
          tmpIndex2 += 2;
        } else if (contents.startsWith("]", tmpIndex2)) {
          if (prematureEnd < 0) {
            endIndex = tmpIndex2;
            hasSecondSquare = true;
          } else {
            endIndex = prematureEnd;
          }
        } else if (tmpIndex2 == maxEndIndex) {
          endIndex = tmpIndex2;
        } else {
          tmpIndex2++;
        }
      }
    } else {
      endIndex = tmpIndex;
      while ((endIndex < maxLength) &&
             (END_CHARACTERS.indexOf(contents.charAt(endIndex)) < 0)) {
        endIndex++;
      }
      return new PageElementExternalLink(
          index, endIndex,
          contents.substring(beginIndex, endIndex),
          null, -1, hasSquare, hasSecondSquare);
    }
    if (endIndex < 0) {
      return null;
    }

    // Find possible description
    int spaceIndex = contents.indexOf(' ', tmpIndex);
    if ((spaceIndex < 0) || (spaceIndex >= endIndex)) {
      return new PageElementExternalLink(
          index, endIndex + (hasSecondSquare ? 1 : 0),
          contents.substring(beginIndex, endIndex),
          null, -1, hasSquare, hasSecondSquare);
    }

    return new PageElementExternalLink(
        index, endIndex + (hasSecondSquare ? 1 : 0),
        contents.substring(beginIndex, spaceIndex),
        contents.substring(spaceIndex + 1, endIndex),
        spaceIndex + 1 - index,
        hasSquare, hasSecondSquare);
  }

  /**
   * @param text Text.
   * @param offset Offset in the text.
   * @return True if the offset in the text is a possible protocol.
   */
  public static boolean isPossibleProtocol(String text, int offset) {
    for (String protocol : privateProtocols) {
      int pos = 0;
      boolean same = true;
      while (same && (pos < protocol.length())) {
        if (offset + pos >= text.length()) {
          same = false;
        } else if (protocol.charAt(pos) != Character.toLowerCase(text.charAt(offset + pos))) {
          same = false;
        } else {
          pos++;
        }
      }
      if (same) {
        return true;
      }
    }
    return false;
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
   * @return Text offset.
   */
  public int getTextOffset() {
    return textOffset;
  }

  /**
   * @return Text.
   */
  public String getTextNotTrimmed() {
    return textNotTrimmed;
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

  /**
   * @return True if the link is in [...] and not in ([...[[...]]...])
   */
  public boolean hasSecondSquare() {
    return hasSecondSquare;
  }

  private PageElementExternalLink(
      int beginIndex, int endIndex,
      String link, String text, int textOffset,
      boolean hasSquare, boolean hasSecondSquare) {
    super(beginIndex, endIndex);
    this.linkNotTrimmed = link;
    this.link = (link != null) ? link.trim() : null;
    this.textNotTrimmed = text;
    this.text = (text != null) ? text.trim() : null;
    this.textOffset = textOffset;
    this.hasSquare = hasSquare;
    this.hasSecondSquare = hasSecondSquare;
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

  /**
   * Create an external link.
   * 
   * @param link Link.
   * @param text Displayed text.
   * @return External link.
   */
  public static String createExternalLink(String link, String text) {
    StringBuilder sb = new StringBuilder();
    sb.append("[");
    if (link != null) {
      sb.append(link);
    }
    if (text != null) {
      sb.append(" ");
      sb.append(text);
    }
    sb.append("]");
    return sb.toString();
  }
}
