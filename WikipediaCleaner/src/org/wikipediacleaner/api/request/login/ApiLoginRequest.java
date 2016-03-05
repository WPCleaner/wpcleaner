/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.login;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.request.ApiRequest;


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
