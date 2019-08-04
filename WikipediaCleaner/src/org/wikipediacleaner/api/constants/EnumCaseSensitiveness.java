/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;

import org.wikipediacleaner.api.data.CharacterUtils;


/**
 * Encapsulate possible rules for Case sensitiveness.
 */
public enum EnumCaseSensitiveness {

  FIRST_LETTER("first-letter"),
  CASE_SENSITIVE("case-sensitive"),
  UNKNOWN("");

  /**
   * Code representing the case sensitiveness.
   */
  private final String code;

  /**
   * @param code Code representing the case sensitiveness.
   */
  private EnumCaseSensitiveness(String code) {
    this.code = code;
  }

  /**
   * Retrieve value matching a given code.
   * 
   * @param code Code.
   * @return Value for the case sensitiveness.
   */
  public static EnumCaseSensitiveness getCase(String code) {
    if (FIRST_LETTER.code.equals(code)) {
      return FIRST_LETTER;
    }
    if (CASE_SENSITIVE.code.equals(code)) {
      return CASE_SENSITIVE;
    }
    return UNKNOWN;
  }

  /**
   * Normalize a text.
   * 
   * @param text Text to be normalized.
   * @return Normalized text.
   */
  public String normalize(String text) {
    if (text == null) {
      return null;
    }
    String result = text.trim();
    result = result.replaceAll("_", " ");
    result = result.replaceAll(" +", " ");
    result = result.trim();
    if (this == FIRST_LETTER) {
      CharacterUtils.ucFirst(result);
    }
    return result;
  }
}
