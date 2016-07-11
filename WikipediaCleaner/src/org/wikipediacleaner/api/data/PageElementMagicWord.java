/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a magic word (__MAGICWORD__). 
 */
public class PageElementMagicWord extends PageElement {

  private final String text;
  private final MagicWord magicWord;

  /**
   * Analyze contents to check if it matches a magic word.
   * 
   * @param wiki Wiki.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementMagicWord analyzeBlock(
      EnumWikipedia wiki, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }
    if ((index < 0) || (index >= contents.length())) {
      return null;
    }

    // Check that it starts as a magic word
    if (!contents.startsWith("__", index)) {
      return null;
    }
    int beginIndex = index;
    index += 2;

    // Find where it ends
    while ((index < contents.length()) &&
           (Character.isLetter(contents.charAt(index)))) {
      index++;
    }

    // Check that it ends as a magic word
    if (!contents.startsWith("__", index)) {
      return null;
    }
    int endIndex = index + 2;

    // Check that it is really a magic word
    String text = contents.substring(beginIndex, endIndex);
    MagicWord magicWord = wiki.getWikiConfiguration().getMagicWordByAlias(text);
    if (magicWord == null) {
      return null;
    }

    return new PageElementMagicWord(
        beginIndex, endIndex,
        text, magicWord);
  }

  public String getText() {
    return text;
  }

  public MagicWord getMagicWord() {
    return magicWord;
  }

  private PageElementMagicWord(
      int beginIndex, int endIndex,
      String text, MagicWord magicWord) {
    super(beginIndex, endIndex);
    this.text = text;
    this.magicWord = magicWord;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return text;
  }
}
