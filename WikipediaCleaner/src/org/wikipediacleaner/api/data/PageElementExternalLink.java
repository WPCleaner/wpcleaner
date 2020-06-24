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
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsComment;
import org.wikipediacleaner.api.data.contents.ContentsUtil;


/**
 * Class containing information about a complete external link ([http://... text]). 
 */
public class PageElementExternalLink extends PageElement {

  private final String linkNotTrimmed;
  private final String link;
  private final int    linkBeginIndex;
  private final int    linkEndIndex;
  private final String textNotTrimmed;
  private final String text;
  private final int    textOffset;
  private final boolean hasSquare;
  private final boolean hasSecondSquare;

  private final static String SEPARATORS_EXCLUDED = " \t\"";

  private final static String SEPARATORS_INCLUDED = "<>|";

  private final static String UNACCEPTABLE = "\n";

  private final static String OUTSIDE_TEMPLATE_SEPARATORS = SEPARATORS_EXCLUDED + SEPARATORS_INCLUDED + UNACCEPTABLE + "[]";

  private final static String IN_TEMPLATES_SEPARATORS = OUTSIDE_TEMPLATE_SEPARATORS + "|}";

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
    if (contents.startsWith("[", tmpIndex)) {
      hasSquare = true;
      tmpIndex++;
    }

    // Possible white spaces characters
    if (hasSquare) {
      tmpIndex = ContentsUtil.moveIndexAfterWhitespace(contents, tmpIndex);
    }

    // Check for protocol
    if (tmpIndex >= maxLength) {
      return null;
    }
    boolean protocolOk = isPossibleProtocol(contents, tmpIndex);
    if (!protocolOk) {
      if (!hasSquare || !contents.startsWith("//", tmpIndex)) {
        return null;
      }
    }
    int beginUrlIndex = tmpIndex;

    // Find if the external link is inside a template
    String fullSeparators = OUTSIDE_TEMPLATE_SEPARATORS;
    if (analysis != null) {
      PageElementTemplate template = analysis.isInTemplate(index);
      if (!hasSquare && (template != null)) {
        fullSeparators = IN_TEMPLATES_SEPARATORS;
      }
    }

    // Find destination of external link
    int endUrlIndex = beginUrlIndex;
    while ((endUrlIndex < maxLength) &&
           (fullSeparators.indexOf(contents.charAt(endUrlIndex)) < 0) &&
           (!contents.startsWith("''", endUrlIndex))) {
      endUrlIndex++;
    }

    // Situations where the external link consists only of the URL
    if (!hasSquare ||
        (endUrlIndex >= maxLength) ||
        (UNACCEPTABLE.indexOf(contents.charAt(endUrlIndex)) >= 0)) {
      return new PageElementExternalLink(
          beginUrlIndex, endUrlIndex,
          contents.substring(beginUrlIndex, endUrlIndex), beginUrlIndex, endUrlIndex,
          null, -1, false, false);
    }
    if ((endUrlIndex < maxLength) &&
        (contents.charAt(endUrlIndex) == ']')) {
      return new PageElementExternalLink(
          index, endUrlIndex + 1,
          contents.substring(beginUrlIndex, endUrlIndex), beginUrlIndex, endUrlIndex,
          null, -1, true, true);
    }

    // Compute maximum index for end of external link
    int maxEndIndex = maxLength;
    if (analysis != null) {
      PageElementTag refTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, index);
      if ((refTag != null) && !refTag.isFullTag() && refTag.isComplete()) {
        maxEndIndex = refTag.getValueEndIndex();
      }
    }

    // Find beginning of text
    int beginTextIndex = endUrlIndex;
    while ((beginTextIndex < maxEndIndex) &&
           (SEPARATORS_EXCLUDED.indexOf(contents.charAt(beginTextIndex)) >= 0)) {
      beginTextIndex++;
    }

    // Find end of text
    int endTextIndex = beginTextIndex;
    int prematureEndIndex = -1;
    int doubleSquareCount = 0;
    while (endTextIndex < maxEndIndex) {
      if (contents.startsWith("[[", endTextIndex)) {
        if (prematureEndIndex < 0) {
          prematureEndIndex = endTextIndex;
        }
        doubleSquareCount++;
        endTextIndex += 2;
      } else if ((doubleSquareCount > 0) && contents.startsWith("]]", endTextIndex)) {
        doubleSquareCount--;
        endTextIndex += 2;
      } else if (contents.charAt(endTextIndex) == ']') {
        if (prematureEndIndex < 0) {
          return new PageElementExternalLink(
              index, endTextIndex + 1,
              contents.substring(beginUrlIndex, endUrlIndex), beginUrlIndex, endUrlIndex,
              contents.substring(beginTextIndex, endTextIndex),
              beginTextIndex - index, true, true);
        }
        return new PageElementExternalLink(
            index, prematureEndIndex,
            contents.substring(beginUrlIndex, endUrlIndex), beginUrlIndex, endUrlIndex,
            contents.substring(beginTextIndex, prematureEndIndex),
            beginTextIndex - index, true, false);
      } else if (UNACCEPTABLE.indexOf(contents.charAt(endTextIndex)) >= 0) {
        return new PageElementExternalLink(
            beginUrlIndex, endUrlIndex,
            contents.substring(beginUrlIndex, endUrlIndex), beginUrlIndex, endUrlIndex,
            null, -1, false, false);
      } else {
        ContentsComment comment = null;
        PageElementTag tagNowiki = null;
        if ((contents.charAt(endTextIndex) == '<') && (analysis != null)) {
          comment = analysis.isInComment(endTextIndex);
          tagNowiki = analysis.isInTag(endTextIndex, PageElementTag.TAG_WIKI_NOWIKI);
        }
        if (comment != null) {
          endTextIndex = comment.getEndIndex();
        } else if (tagNowiki != null) {
          endTextIndex = tagNowiki.getCompleteEndIndex();
        } else {
          endTextIndex++;
        }
      }
    }

    // No end found
    return new PageElementExternalLink(
        beginUrlIndex, endUrlIndex,
        contents.substring(beginUrlIndex, endUrlIndex), beginUrlIndex, endUrlIndex,
        null, -1, false, false);
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
   * @return Begin index of the external link.
   */
  public int getLinkBeginIndex() {
    return linkBeginIndex;
  }

  /**
   * @return End index of the external link.
   */
  public int getLinkEndIndex() {
    return linkEndIndex;
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

  /**
   * Constructor.
   * 
   * @param beginIndex Begin index of the entire external link.
   * @param endIndex End index of the entire external link.
   * @param link Target of the link.
   * @param linkBeginIndex Begin index of the target of the link.
   * @param linkEndIndex End index of the target of the link.
   * @param text Text of the link.
   * @param textOffset Offset of the text.
   * @param hasSquare True if there's an open square bracket.
   * @param hasSecondSquare True if there's also an close square bracket.
   */
  private PageElementExternalLink(
      int beginIndex, int endIndex,
      String link, int linkBeginIndex, int linkEndIndex,
      String text, int textOffset,
      boolean hasSquare, boolean hasSecondSquare) {
    super(beginIndex, endIndex);
    this.linkNotTrimmed = link;
    String tmpLink = (link != null) ? link.trim() : null;
    if ((tmpLink != null) && (tmpLink.startsWith("//"))) {
      tmpLink = "http:" + tmpLink;
    }
    this.link = tmpLink;
    this.linkBeginIndex = linkBeginIndex;
    this.linkEndIndex = linkEndIndex;
    if ((text != null) && (text.trim().length() > 0)) {
      this.textNotTrimmed = text;
      this.text = text.trim();
    } else {
      this.textNotTrimmed = null;
      this.text = null;
    }
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
