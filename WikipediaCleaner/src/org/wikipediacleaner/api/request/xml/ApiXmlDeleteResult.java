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
import org.wikipediacleaner.api.request.ApiDeleteResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML delete results.
 */
public class ApiXmlDeleteResult extends ApiXmlResult implements ApiDeleteResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlDeleteResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute delete request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  @Override
  public void executeDelete(
      Map<String, String> properties)
          throws APIException {
    try {
      checkForError(getRoot(properties, ApiRequest.MAX_ATTEMPTS));
    } catch (JDOMParseException e) {
      log.error("Error deleting page", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
