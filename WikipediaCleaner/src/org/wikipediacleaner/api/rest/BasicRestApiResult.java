/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.rest;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.http.hc3.Hc3HttpUtils;


/**
 * MediaWiki REST API results.
 */
public abstract class BasicRestApiResult implements RestApiResult {

  /**
   * Logger.
   */
  protected final Logger log = LoggerFactory.getLogger(BasicRestApiResult.class);

  /**
   * Wiki on which request are made.
   */
  private final EnumWikipedia wiki;

  /**
   * HTTP client for making requests.
   */
  private final HttpClient httpClient;

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  protected BasicRestApiResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    this.wiki = wiki;
    this.httpClient = httpClient;
  }

  /**
   * @return Wiki on which requests are made.
   */
  @Override
  public EnumWikipedia getWiki() {
    return wiki;
  }

  /**
   * @return HTTP client for making requests.
   */
  @Override
  public HttpClient getHttpClient() {
    return httpClient;
  }

  // ==========================================================================
  // HTTP management
  // ==========================================================================

  /**
   * Create an HttpMethod.
   * 
   * @param properties Properties to drive the API.
   * @param path Path to REST API method.
   * @param param Parameter for REST API method.
   * @return HttpMethod.
   */
  protected HttpMethod createHttpMethod(
      Map<String, String> properties, String path, String param) {
    String encodedPath = path;
    if ((param != null) && !param.isEmpty()) {
      encodedPath += "/" + param;
      // NOTE: Do not encode, doesn't work with titles like Sergueï Chakourov
      /*try {
        encodedPath += "/" + URLEncoder.encode(param, "UTF8");
      } catch (UnsupportedEncodingException e) {
        // Nothing
      }*/
    }
    return Hc3HttpUtils.createHttpMethod(
        getWiki().getSettings().getHostURL(true) + "/" + encodedPath,
        properties, false);
  }
}
