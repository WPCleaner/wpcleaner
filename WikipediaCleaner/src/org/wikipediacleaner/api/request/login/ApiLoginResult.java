/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.login;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API login results.
 */
public interface ApiLoginResult extends ApiResult {

  /**
   * Execute login request.
   * 
   * @param properties Properties defining request.
   * @return Login result.
   * @throws APIException Exception thrown by the API.
   */
  public LoginResult executeLogin(Map<String, String> properties) throws APIException;
}
