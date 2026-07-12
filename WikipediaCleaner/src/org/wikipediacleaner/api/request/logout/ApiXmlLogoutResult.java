/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.logout;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiXmlResult;

/**
 * MediaWiki API XML logout results.
 */
public class ApiXmlLogoutResult extends ApiXmlResult implements ApiLogoutResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLogoutResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute logout request.
   * 
   * @param properties Properties defining request.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void executeLogout(
      Map<String, String> properties)
          throws APIException {
    getRoot(properties, 1);
  }

  /**
   * @return True if identification parameters should be sent.
   */
  @Override
  protected boolean shouldSendIdentification() {
    return true;
  }
}
