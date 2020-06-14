/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.contents.ContentsUtil;


/**
 * Class containing information about a language link ([[lang:link|text]]). 
 */
public class PageElementLanguageLink extends PageElement {

  private final String languageNotTrimmed;
  private final String language;
  private final String linkNotTrimmed;
  private final String link;
  private final String textNotTrimmed;
  private final String text;

  /**
   * Analyze contents to check if it matches a language link.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementLanguageLink analyzeBlock(
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
    tmpIndex = ContentsUtil.moveIndexAfterWhitespace(contents, tmpIndex);

    // Search for :
    tmpIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex, ":|]|");
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != ':')) {
      return null;
    }

    // Check that namespace is language
    int colonIndex = tmpIndex;
    List<Language> languages = wikipedia.getWikiConfiguration().getLanguages();
    String language = contents.substring(beginIndex, colonIndex).trim();
    if (!Language.isLanguageCode(languages, language)) {
      return null;
    }
    if (language.equals(wikipedia.getSettings().getLanguage())) {
      return null;
    }

    // Search for |
    tmpIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex, "|]");
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Simple language tag [[lang:link]]
    if (contents.charAt(tmpIndex) == ']') {
      if (!contents.startsWith("]]", tmpIndex)) {
        return null;
      }
      return new PageElementLanguageLink(
          index, tmpIndex + 2,
          contents.substring(beginIndex, colonIndex),
          contents.substring(colonIndex + 1, tmpIndex),
          null);
    }

    // Find elements of image
    int endIndex = contents.indexOf("]]", colonIndex);
    if (endIndex < 0) {
      return null;
    }
    return new PageElementLanguageLink(
        index, endIndex + 2,
        contents.substring(beginIndex, colonIndex),
        contents.substring(colonIndex + 1, tmpIndex),
        contents.substring(tmpIndex + 1, endIndex));
  }

  public String getLanguage() {
    return language;
  }

  public String getLink() {
    return link;
  }

  public String getText() {
    return text;
  }

  private PageElementLanguageLink(
      int beginIndex, int endIndex,
      String language, String link,
      String text) {
    super(beginIndex, endIndex);
    this.languageNotTrimmed = language;
    this.language = (language != null) ? language.trim() : null;
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
    sb.append("[[");
    sb.append(languageNotTrimmed);
    sb.append(':');
    if (linkNotTrimmed != null) {
      sb.append(linkNotTrimmed);
    }
    if (textNotTrimmed != null) {
      sb.append('|');
      sb.append(textNotTrimmed);
    }
    sb.append("]]");
    return sb.toString();
  }
}
