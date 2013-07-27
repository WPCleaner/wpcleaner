/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;


/**
 * A basic interface for checking that a String follow some rules.
 */
public interface StringChecker {

  /**
   * Holder for result.
   */
  public static class Result {

    /**
     * Is the result OK ?
     */
    private final boolean ok;

    /**
     * What is the text result ?
     */
    private final String text;

    /**
     * Explanation if the result is not OK.
     */
    private final String message;

    /**
     * @param ok Is the result OK ?
     * @param text Text result.
     * @param message Explanation.
     */
    public Result(boolean ok, String text, String message) {
      this.ok = ok;
      this.text = text;
      this.message = message;
    }

    /**
     * @return Is the result OK ?
     */
    public boolean isOk() {
      return ok;
    }

    /**
     * @return Text.
     */
    public String getText() {
      return text;
    }

    /**
     * @return Explanation.
     */
    public String getMessage() {
      return message;
    }
  }

  /**
   * Check if a text follows the rules.
   * 
   * @param text Text to check.
   * @return Result.
   */
  public Result checkString(String text);
}
