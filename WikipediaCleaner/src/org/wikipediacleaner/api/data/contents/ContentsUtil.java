/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;


/**
 * Utility class for contents.
 */
public class ContentsUtil {

  /** Whitespace characters to trim around */
  private final static String TRIM_WHITESPACE = " \u00A0";

  /**
   * @param text Text to be trimmed.
   * @return Text trimmed for all its white space characters (more thorough than String.trim()).
   */
  public static String trimWhitespace(String text) {
    if (text == null) {
      return "";
    }
    int endIndex = text.length();
    while ((endIndex > 0) &&
           (TRIM_WHITESPACE.indexOf(text.charAt(endIndex - 1)) >= 0)) {
      endIndex--;
    }
    int beginIndex = 0;
    while ((beginIndex < endIndex) &&
        (TRIM_WHITESPACE.indexOf(text.charAt(beginIndex)) >= 0)) {
      beginIndex++;
    }
    if ((beginIndex == 0) && (endIndex == text.length())) {
      return text;
    }
    return text.substring(beginIndex, endIndex);
  }

  // ==========================================================================
  // Search characters in text
  // ==========================================================================

  /**
   * Find the last character from a set which is before a given index in a text.
   * 
   * @param contents Text to search into.
   * @param startIndex Start index to search in the text.
   * @param characters Set of characters to look for.
   * @return Maximum index in the text before the start index for a character in the given set.
   *         If no character is found, -1 is returned.
   */
  public static int getLastIndexInSetBefore(String contents, int startIndex, String characters) {
    if ((contents == null) ||
        (characters == null) ||
        (characters.isEmpty())) {
      return -1;
    }
    int tmpIndex = startIndex - 1;
    while (tmpIndex >= 0) {
      if (characters.indexOf(contents.charAt(tmpIndex)) >= 0) {
        return tmpIndex;
      }
      tmpIndex--;
    }
    return -1;
  }

  /**
   * Find the last character not from a set which is before a given index in a text.
   * 
   * @param contents Text to search into.
   * @param startIndex Start index to search in the text.
   * @param characters Set of characters to avoid.
   * @return Maximum index in the text before the start index for a character not in the given set.
   *         If no character is found, -1 is returned.
   */
  public static int getLastIndexNotInSetBefore(String contents, int startIndex, String characters) {
    if ((contents == null) ||
        (characters == null) ||
        (characters.isEmpty())) {
      return -1;
    }
    int tmpIndex = startIndex - 1;
    while (tmpIndex >= 0) {
      if (characters.indexOf(contents.charAt(tmpIndex)) < 0) {
        return tmpIndex;
      }
      tmpIndex--;
    }
    return -1;
  }

  /**
   * Find the first character from a set which is after a given index in a text.
   * 
   * @param contents Text to search into.
   * @param startIndex Start index to search in the text.
   * @param characters Set of characters to look for.
   * @return Minimum index in the text after the start index for a character in the given set.
   *         If no character is found, -1 is returned.
   */
  public static int getFirstIndexInSetAfter(String contents, int startIndex, String characters) {
    if ((contents == null) ||
        (characters == null) ||
        (characters.isEmpty())) {
      return -1;
    }
    int tmpIndex = startIndex;
    while (tmpIndex < contents.length()) {
      if (characters.indexOf(contents.charAt(tmpIndex)) >= 0) {
        return tmpIndex;
      }
      tmpIndex++;
    }
    return -1;
  }

  /**
   * Find the first character not from a set which is after a given index in a text.
   * 
   * @param contents Text to search into.
   * @param startIndex Start index to search in the text.
   * @param characters Set of characters to avoid.
   * @return Minimum index in the text after the start index for a character not in the given set.
   *         If no character is found, -1 is returned.
   */
  public static int getFirstIndexNotInSetAfter(String contents, int startIndex, String characters) {
    if ((contents == null) ||
        (characters == null) ||
        (characters.isEmpty())) {
      return -1;
    }
    int tmpIndex = startIndex;
    while (tmpIndex < contents.length()) {
      if (characters.indexOf(contents.charAt(tmpIndex)) < 0) {
        return tmpIndex;
      }
      tmpIndex++;
    }
    return -1;
  }

  /**
   * Find the beginning of a line containing a given index in a text.
   * 
   * @param contents Text to search into.
   * @param index Index for which we want the beginning of the line.
   * @return Beginning of the line containing the index.
   */
  public static int getLineBeginIndex(String contents, int index) {
    int tmpIndex = getLastIndexInSetBefore(contents, index, "\n");
    return tmpIndex + 1;
  }

  /**
   * Find the end of a line containing a given index in a text.
   * 
   * @param contents Text to search into.
   * @param index Index for which we want the end of the line.
   * @return End of the line containing the index.
   */
  public static int getLineEndIndex(String contents, int index) {
    int tmpIndex = getFirstIndexInSetAfter(contents, index, "\n");
    if ((tmpIndex < 0) && (contents != null)) {
      return contents.length();
    }
    return tmpIndex;
  }

  /**
   * Count characters in a text.
   * 
   * @param contents Text to search into.
   * @param beginIndex Beginning index for the area to search into.
   * @param endIndex End index for the area to search into.
   * @param characters Set of characters to look for.
   * @return Number of characters from the set in the area of text.
   */
  public static int countCharacters(String contents, int beginIndex, int endIndex, String characters) {
    if ((contents == null) || (characters == null)) {
      return 0;
    }
    int count = 0;
    for (int index = beginIndex; index < endIndex; index++) {
      if (characters.indexOf(contents.charAt(index)) >= 0) {
        count++;
      }
    }
    return count;
  }
}
