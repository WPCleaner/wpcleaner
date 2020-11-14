/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.CharacterUtils;

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
  public static @Nonnull String trimWhitespace(@Nullable String text) {
    if (text == null) {
      return StringUtils.EMPTY;
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
  // Search characters in text - Forward
  // ==========================================================================

  /**
   * Move forward the index in the text while the character at the index is from the given set.
   * 
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @param set Set of characters to look for.
   * @return Minimum value of index (≥ startIndex) with a character not from the given set.
   *         If all characters after startIndex are from the given set, returns contents.length().
   */
  public static int moveIndexForwardWhileFound(
      @Nullable String contents,
      int startIndex,
      @Nullable String set) {
    if ((contents == null) || (startIndex < 0)) {
      return -1;
    }
    if ((set == null) || set.isEmpty()) {
      return startIndex;
    }
    int tmpIndex = startIndex;
    while (tmpIndex < contents.length()) {
      if (set.indexOf(contents.charAt(tmpIndex)) < 0) {
        return tmpIndex;
      }
      tmpIndex++;
    }
    return tmpIndex;
  }

  /**
   * Move forward the index in the text while the character at the index is not from the given set.
   * 
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @param set Set of characters to look for.
   * @return Minimum value of index (≥ startIndex) after all characters from the given set.
   *         If all characters after startIndex are not from the given set, returns contents.length().
   */
  public static int moveIndexForwardWhileNotFound(
      @Nullable String contents,
      int startIndex,
      @Nullable String set) {
    if ((contents == null) || (startIndex < 0)) {
      return -1;
    }
    if ((set == null) || set.isEmpty()) {
      return contents.length();
    }
    int tmpIndex = startIndex;
    while (tmpIndex < contents.length()) {
      if (set.indexOf(contents.charAt(tmpIndex)) >= 0) {
        return tmpIndex;
      }
      tmpIndex++;
    }
    return tmpIndex;
  }

  /**
   * Move forward the index in the text while the character is a whitespace.
   * 
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @return Minimum value of index (≥ startIndex) after whitespace characters.
   *         If all characters after startIndex are whitespace, returns contents.length().
   */
  public static int moveIndexAfterWhitespace(
      @Nullable String contents,
      int startIndex) {
    return moveIndexForwardWhileFound(contents, startIndex, CharacterUtils.WHITESPACE);
  }

  /**
   * Find the end of a line containing a given index in a text.
   * 
   * @param contents Text to search into.
   * @param index Index for which we want the end of the line.
   * @return End of the line containing the index.
   */
  public static int getLineEndIndex(@Nullable String contents, int index) {
    return moveIndexForwardWhileNotFound(contents, index, "\n");
  }

  // ==========================================================================
  // Search characters in text - Backward
  // ==========================================================================

  /**
   * Move backward the index in the text while the character at the index is from the given set.
   * 
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @param set Set of characters to look for.
   * @return Maximum value of index (≤ startIndex) with a character not from the given set.
   *         If all characters before startIndex are from the given set, returns -1.
   */
  public static int moveIndexBackwardWhileFound(
      @Nullable String contents,
      int startIndex,
      @Nullable String set) {
    if ((contents == null) || (startIndex < 0)) {
      return -1;
    }
    if ((set == null) || set.isEmpty()) {
      return startIndex;
    }
    int tmpIndex = startIndex;
    while (tmpIndex >= 0) {
      if (set.indexOf(contents.charAt(tmpIndex)) < 0) {
        return tmpIndex;
      }
      tmpIndex--;
    }
    return tmpIndex;
  }

  /**
   * Move backward the index in the text while the character at the index is not from the given set.
   * 
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @param set Set of characters to look for.
   * @return Maximum value of index (≤ startIndex) before all characters from the given set.
   *         If all characters before startIndex are not from the given set, returns -1.
   */
  public static int moveIndexBackwardWhileNotFound(
      @Nullable String contents,
      int startIndex,
      @Nullable String set) {
    if ((contents == null) || (startIndex < 0)) {
      return -1;
    }
    if ((set == null) || set.isEmpty()) {
      return -1;
    }
    int tmpIndex = startIndex;
    while (tmpIndex >= 0) {
      if (set.indexOf(contents.charAt(tmpIndex)) >= 0) {
        return tmpIndex;
      }
      tmpIndex--;
    }
    return tmpIndex;
  }

  /**
   * Move backward the index in the text while the character is a whitespace.
   * 
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @return Maximum value of index (≤ startIndex) before whitespace characters.
   *         If all characters before startIndex are whitespace, returns -1.
   */
  public static int moveIndexBeforeWhitespace(
      @Nullable String contents,
      int startIndex) {
    return moveIndexBackwardWhileFound(contents, startIndex, CharacterUtils.WHITESPACE);
  }

  /**
   * Find the beginning of a line containing a given index in a text.
   * 
   * @param contents Text to search into.
   * @param index Index for which we want the beginning of the line.
   * @return Beginning of the line containing the index.
   */
  public static int getLineBeginIndex(@Nullable String contents, int index) {
    return moveIndexBackwardWhileNotFound(contents, index, "\n") + 1;
  }

  // ==========================================================================
  // Characters management
  // ==========================================================================

  /**
   * Count characters in a text.
   * 
   * @param contents Text to search into.
   * @param beginIndex Beginning index for the area to search into.
   * @param endIndex End index for the area to search into.
   * @param characters Set of characters to look for.
   * @return Number of characters from the set in the area of text.
   */
  public static int countCharacters(
      @Nullable String contents,
      int beginIndex, int endIndex,
      @Nullable String characters) {
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

  // ==========================================================================
  // String comparison
  // ==========================================================================

  /**
   * Check if a string starts with a given prefix (ignoring case) at a given offset.
   * 
   * @param str String to check.
   * @param prefix Prefix to check.
   * @param offset Offset in the string.
   * @return True if the string starts with the prefix ignoring case at the offset.
   */
  public static boolean startsWithIgnoreCase(
      @Nonnull String str,
      @Nonnull String prefix,
      int offset) {
    if (offset + prefix.length() > str.length()) {
      return false;
    }
    for (int index = 0; index < prefix.length(); index++) {
      if (Character.toLowerCase(str.charAt(offset + index)) != Character.toLowerCase(prefix.charAt(index))) {
        return false;
      }
    }
    return true;
  }
}
