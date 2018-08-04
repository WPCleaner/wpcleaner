/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.logout;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API logout results.
 */
public interface ApiLogoutResult extends ApiResult {

  /**
   * Execute logout request.
   * 
   * @param properties Properties defining request.
   * @throws APIException Exception thrown by the API.
   */
  public void executeLogout(Map<String, String> properties) throws APIException;
}
