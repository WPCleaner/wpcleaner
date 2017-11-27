/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a complete interwiki link ([[iw:link#anchor|text]]). 
 */
public class PageElementInterwikiLink extends PageElement {

  private final Interwiki interwiki;
  private final String interwikiTextNotTrimmed;
  private final String interwikiText;
  private final String linkNotTrimmed;
  private final String link;
  private final String anchorNotTrimmed;
  private final String anchor;
  private final String textNotTrimmed;
  private final String text;
  private final int    textOffset;

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

    // Possible whitespace characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Possible colon at the beginning
    boolean beginWithColon = false;
    if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ':')) {
      beginWithColon = true;
      tmpIndex++;
      beginIndex = tmpIndex;
      while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
        tmpIndex++;
      }
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
    List<Interwiki> interwikis = wikipedia.getWikiConfiguration().getInterwikis();
    if (interwikis == null) {
      return null;
    }
    Interwiki interwiki = null;
    for (Interwiki tmpInterwiki : interwikis) {
      if ((tmpInterwiki != null) &&
          ((tmpInterwiki.getLanguage() == null) ||
           (tmpInterwiki.getLanguage().length() == 0) ||
           beginWithColon) &&
          (interwikiText.equals(tmpInterwiki.getPrefix()))) {
        if (interwiki == null) {
          interwiki = tmpInterwiki;
        } else if (tmpInterwiki.getLanguage().length() < interwiki.getLanguage().length()) {
          interwiki = tmpInterwiki;
        }
      }
    }
    if (interwiki == null) {
      return null;
    }

    int anchorIndex = PageContents.findCharacter(contents, '#', tmpIndex, endIndex);
    int pipeIndex = PageContents.findCharacter(contents, '|', tmpIndex, endIndex);

    // Create interwiki link
    if ((pipeIndex >= 0) && (pipeIndex < endIndex)) {
      if ((anchorIndex >= 0) && (anchorIndex < pipeIndex)) {
        return new PageElementInterwikiLink(
            index, endIndex + 2,
            interwiki, interwikiText,
            contents.substring(colonIndex + 1, anchorIndex),
            contents.substring(anchorIndex + 1, pipeIndex),
            contents.substring(pipeIndex + 1, endIndex),
            pipeIndex + 1 - index);
      }
      return new PageElementInterwikiLink(
          index, endIndex + 2,
          interwiki, interwikiText,
          contents.substring(colonIndex + 1, pipeIndex),
          null,
          contents.substring(pipeIndex + 1, endIndex),
          pipeIndex + 1 - index);
    }
    if ((anchorIndex >= 0) && (anchorIndex < endIndex)) {
      return new PageElementInterwikiLink(
          index, endIndex + 2,
          interwiki, interwikiText,
          contents.substring(colonIndex + 1, anchorIndex),
          contents.substring(anchorIndex + 1, endIndex),
          null, -1);
    }
    return new PageElementInterwikiLink(
        index, endIndex + 2,
        interwiki, interwikiText,
        contents.substring(colonIndex + 1, endIndex),
        null, null, -1);
  }

  public Interwiki getInterwiki() {
    return interwiki;
  }

  public String getInterwikiText() {
    return interwikiText;
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

  public int getTextOffset() {
    return textOffset;
  }

  /**
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @param interwiki Interwiki.
   * @param interwikiText Interwiki text.
   * @param link Link.
   * @param anchor Anchor.
   * @param text Text.
   * @param textOffset Offset of the text.
   */
  private PageElementInterwikiLink(
      int beginIndex, int endIndex,
      Interwiki interwiki, String interwikiText,
      String link, String anchor,
      String text, int textOffset) {
    super(beginIndex, endIndex);
    this.interwiki = interwiki;
    this.interwikiTextNotTrimmed = interwikiText;
    this.interwikiText = (interwikiText != null) ? interwikiText.trim() : null;
    this.linkNotTrimmed = link;
    this.link = (link != null) ? link.trim() : null;
    this.anchorNotTrimmed = anchor;
    this.anchor = (anchor != null) ? anchor.trim() : null;
    this.textNotTrimmed = text;
    this.text = (text != null) ? text.trim() : null;
    this.textOffset = textOffset;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    sb.append(interwikiTextNotTrimmed);
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

  /**
   * Create an interwiki link.
   * 
   * @param interwiki Interwiki.
   * @param link Link.
   * @param anchor Anchor
   * @param text Displayed text.
   * @return Internal link.
   */
  public static String createInterwikiLink(
      String interwiki, String link, String anchor, String text) {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    String fullLink = null;
    if ((interwiki != null) || (link != null) || (anchor != null)) {
      fullLink =
          ((interwiki != null) ? interwiki.trim() + ":" : "") +
          ((link != null) ? link.trim() : "") +
          ((anchor != null) ? ("#" + anchor.trim()) : "");
    }
    if (text != null) {
      if ((fullLink != null) && (!Page.areSameTitle(fullLink, text))) {
        sb.append(fullLink);
        sb.append("|");
      }
      sb.append(text.trim());
    } else {
      sb.append(fullLink);
    }
    sb.append("]]");
    return sb.toString();
  }
}
