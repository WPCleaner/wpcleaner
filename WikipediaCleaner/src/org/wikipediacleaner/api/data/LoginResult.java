/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumLoginResult;


/**
 * Class containing the information about the login result.
 */
public class LoginResult {

  private final EnumLoginResult loginResult;
  private final String details;
  private final String wait;

  /**
   * Create a correct login result.
   * 
   * @return Login result.
   */
  static public LoginResult createCorrectLogin() {
    return new LoginResult(EnumLoginResult.SUCCESS, null, null);
  }

  /**
   * Create a login result to indicate that a token is needed.
   * 
   * @param token Token.
   * @return Login result.
   */
  static public LoginResult createNeedTokenLogin(String token) {
    return new LoginResult(EnumLoginResult.NEED_TOKEN, token, null);
  }

  /**
   * Create a login result to indicate that the login is aborted.
   *
   * @param detail Detail.
   * @return Login result.
   */
  static public LoginResult createAbortedLogin(String detail) {
    return new LoginResult(
        EnumLoginResult.ABORTED,
        """
        Login with main account is not supported, obtain credentials Special:BotPasswords.
        See https://en.wikipedia.org/wiki/Wikipedia:WPCleaner/BotPasswords for detailed explanations.
        %s
        """.formatted(detail), null);
  }

  /**
   * Create an error login result.
   * 
   * @param errorText Text error.
   * @param details Details of the error.
   * @param wait Wait text.
   * @return Error.
   */
  static public LoginResult createErrorLogin(
      String errorText, String details, String wait) {
    EnumLoginResult loginResult = EnumLoginResult.getEnumByCode(errorText);
    return new LoginResult(loginResult, details, wait);
  }
  
  private LoginResult(EnumLoginResult loginResult, String details, String wait) {
    this.loginResult = loginResult;
    this.details = details;
    this.wait = wait;
  }

  /**
   * @return Flag indicating if login is successful.
   */
  public boolean isLoginSuccessful() {
    if (loginResult != null) {
      return loginResult.isOk();
    }
    return false;
  }

  /**
   * @return Flag indicating if token is needed.
   */
  public boolean isTokenNeeded() {
    if (loginResult != null) {
      return (loginResult == EnumLoginResult.NEED_TOKEN);
    }
    return false;
  }

  public String getDetails() {
    return details;
  }
  
  public String getWait() {
    return wait;
  }
  
  @Override
  public String toString() {
    return ((loginResult != null) ? loginResult.getText() : "") +
           ((details != null) ? " - " + details : "");
  }
}
