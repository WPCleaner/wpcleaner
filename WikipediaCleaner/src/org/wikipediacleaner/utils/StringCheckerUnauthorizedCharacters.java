/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import org.wikipediacleaner.i18n.GT;


/**
 * Checking that a String doesn't contain unauthorized characters.
 */
public class StringCheckerUnauthorizedCharacters implements StringChecker {

  /**
   * String containing all unauthorized characters.
   */
  private final String unauthorized;

  /**
   * @param unauthorized Unauthorized characters.
   */
  public StringCheckerUnauthorizedCharacters(String unauthorized) {
    this.unauthorized = unauthorized;
  }

  /**
   * Check if a text contains no unauthorized characters.
   * 
   * @param text Text to check.
   * @return Result.
   */
  @Override
  public Result checkString(String text) {
    if ((unauthorized == null) ||
        (unauthorized.length() == 0) ||
        (text == null) ||
        (text.length() == 0)) {
      return new Result(true, text, null);
    }
    StringBuilder buffer = new StringBuilder(text.length());
    boolean ok = true;
    for (int index = 0; index < text.length(); index++) {
      if (unauthorized.indexOf(text.charAt(index)) < 0) {
        buffer.append(text.charAt(index));
      } else {
        ok = false;
      }
    }
    return new Result(ok, buffer.toString(), GT._T(
        "The value cannot contain any of these characters: {0}", unauthorized));
  }

}
