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
}
