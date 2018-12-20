/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;


/**
 * Encapsulate possible login results.
 */
public enum EnumLoginResult {

  ABORTED          ("Aborted",         "Login aborted"),
  EMPTY_PASS       ("EmptyPass",       "Empty password"),
  ILLEGAL          ("Illegal",         "Illegal"),
  NEED_TOKEN       ("NeedToken",       "Need token"),
  NEED_TO_WAIT     ("NeedToWait",      "Need to wait"),
  NO_NAME          ("NoName",          "No name"),
  NOT_EXISTS       ("NotExists",       "Not exists"),
  SUCCESS          (null,              "Login successful"),
  WRONG_PASSWORD   ("WrongPass",       "Wrong password"),
  WRONG_PLUGIN_PASS("WrongPluginPass", "Wrong plugin password"),
  
  UNKNOWN_ERROR    (null,              "Unknown error");

  private final String code;
  private final String text;

  /**
   * @param text Associated text
   */
  EnumLoginResult(String code, String text) {
    this.code = code;
    this.text = text;
  }

  /**
   * @param code Error code.
   * @return Matching EnumLoginResult.
   */
  public static EnumLoginResult getEnumByCode(String code) {
    for (EnumLoginResult result : EnumLoginResult.values()) {
      if ((result != null) &&
          (result.code != null) &&
          (result.code.equalsIgnoreCase(code))) {
        return result;
      }
    }
    return UNKNOWN_ERROR;
  }

  /**
   * @return Login successful ?
   */
  public boolean isOk() {
    return equals(SUCCESS);
  }

  /**
   * @return Associated code.
   */
  public String getCode() {
    return code;
  }

  /**
   * @return Associated text.
   */
  public String getText() {
    return text;
  }
}
