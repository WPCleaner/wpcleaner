/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.input.JDOMParseException;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiLogoutResult;

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
   * @throws APIException
   */
  @Override
  public void executeLogout(
      Map<String, String> properties)
          throws APIException {
    try {
      getRoot(properties, 1);
    } catch (JDOMParseException e) {
      log.error("Exception in MediaWikiAPI.logout()", e);
      throw new APIException("Couldn't logout");
    }
  }

  /**
   * @return True if identification parameters should be sent.
   */
  @Override
  protected boolean shouldSendIdentification() {
    return true;
  }
}
