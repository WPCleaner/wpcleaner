/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.logout;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API logout requests.
 */
public class ApiLogoutRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLogoutResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLogoutRequest(EnumWikipedia wiki, ApiLogoutResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Logout.
   * 
   * @throws APIException Exception thrown by the API.
   */
  public void logout() throws APIException {
    Map<String, String> properties = getProperties(ACTION_LOGOUT, result.getFormat());
    result.executeLogout(properties);
  }
}
