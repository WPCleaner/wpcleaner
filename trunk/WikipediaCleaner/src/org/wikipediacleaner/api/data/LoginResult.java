/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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
 * Class containing the informations about the login result.
 */
public class LoginResult {

  private EnumLoginResult loginResult;
  private String details;
  private String wait;

  static public LoginResult createCorrectLogin() {
    return new LoginResult(EnumLoginResult.SUCCESS, null, null);
  }
  
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

  public boolean isLoginSuccessful() {
    if (loginResult != null) {
      return loginResult.isOk();
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
