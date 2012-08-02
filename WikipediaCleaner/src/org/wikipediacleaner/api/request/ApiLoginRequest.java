/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.api.request;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LoginResult;


/**
 * MediaWiki API login requests.
 */
public class ApiLoginRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Name.
   */
  public final static String PROPERTY_NAME = "lgname";

  /**
   * Property for Password.
   */
  public final static String PROPERTY_PASSWORD = "lgpassword";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "lgtoken";

  /**
   * Property for User Name.
   */
  public final static String PROPERTY_USER_NAME = "lgusername";

  /**
   * Property for User Id.
   */
  public final static String PROPERTY_USER_ID = "lguserid";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLoginResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLoginRequest(EnumWikipedia wiki, ApiLoginResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Login.
   * 
   * @param username User name.
   * @param password Password.
   * @return Login result.
   */
  public LoginResult login(String username, String password) throws APIException {
    Map<String, String> properties = getProperties(ACTION_LOGIN, result.getFormat());
    properties.put(PROPERTY_NAME, username);
    properties.put(PROPERTY_PASSWORD, password);
    LoginResult loginResult = result.executeLogin(properties);
    return loginResult;
  }
}
