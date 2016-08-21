/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.purge;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.input.JDOMParseException;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML purge results.
 */
public class ApiXmlPurgeResult extends ApiXmlResult implements ApiPurgeResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlPurgeResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute purge request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  @Override
  public void executePurge(
      Map<String, String> properties)
          throws APIException {
    try {
      checkForError(getRoot(properties, ApiRequest.MAX_ATTEMPTS));
    } catch (JDOMParseException e) {
      log.error("Error purging page cache", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
