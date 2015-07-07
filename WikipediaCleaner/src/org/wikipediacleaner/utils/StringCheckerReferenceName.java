/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import org.wikipediacleaner.i18n.GT;


/**
 * Checking that a String can be used to name a reference.
 */
public class StringCheckerReferenceName implements StringChecker {

  /**
   * String containing all unauthorized characters.
   */
  private final static String unauthorized = "[]\"";

  /**
   * Construct a reference name checker
   */
  public StringCheckerReferenceName() {
    //
  }

  /**
   * Check if a text can be used to name a reference.
   * 
   * @param text Text to check.
   * @return Result.
   */
  @Override
  public Result checkString(String text) {
    if ((unauthorized == null) ||
        (unauthorized.length() == 0) ||
        (text == null) ||
        (text.trim().length() == 0)) {
      return new Result(true, text, null);
    }
    text = text.trim();
    StringBuilder buffer = new StringBuilder(text.length());
    boolean ok = true;
    boolean onlyDigits = true;
    for (int index = 0; index < text.length(); index++) {
      if (unauthorized.indexOf(text.charAt(index)) < 0) {
        buffer.append(text.charAt(index));
        if (!Character.isDigit(text.charAt(index))) {
          onlyDigits = false;
        }
      } else {
        ok = false;
      }
    }
    if (ok && onlyDigits) {
      return new Result(false, buffer.toString(), GT._(
          "A reference name cannot be a numeric key"));
    }
    return new Result(ok, buffer.toString(), GT._(
        "A reference name cannot contain any of these characters: {0}", unauthorized));
  }

}
