/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumLoginResult;


/**
 * Class containing the information about the login result.
 */
public class LoginResult {

  private EnumLoginResult loginResult;
  private String details;
  private String wait;

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
