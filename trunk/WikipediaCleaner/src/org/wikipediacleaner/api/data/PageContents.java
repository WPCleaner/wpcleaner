/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * Utility class to manage page contents.
 */
public class PageContents {

  // ==========================================================================
  // General methods
  // ==========================================================================

  /**
   * Find the first occurrence of a character in a substring.
   * 
   * @param text String.
   * @param character Character.
   * @param begin Beginning of the substring.
   * @param end End of the substring.
   * @return First occurrence of character.
   */
  public static int findCharacter(
      String text, char character, int begin, int end) {
    if (text == null) {
      return -1;
    }
    for (int i = begin; i < end; i++) {
      if (text.charAt(i) == character) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Expand text for a page (for example, replacing {{PAGENAME}}).
   * 
   * @param page Page.
   * @param text Text to expand.
   * @return Expanded text.
   */
  public static String expandText(Page page, String text) {
    if ((page == null) || (text == null)) {
      return text;
    }
    String result = text;
    result = result.replaceAll("\\{\\{PAGENAME\\}\\}", page.getValuePAGENAME());
    return result;
  }
}
