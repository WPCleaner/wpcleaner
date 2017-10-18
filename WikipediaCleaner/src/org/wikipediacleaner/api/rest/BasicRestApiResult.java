/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.rest;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.HttpUtils;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * MediaWiki REST API results.
 */
public abstract class BasicRestApiResult implements RestApiResult {

  /**
   * Logger.
   */
  protected final Log log = LogFactory.getLog(BasicRestApiResult.class);

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
   * @return HttpMethod.
   */
  protected HttpMethod createHttpMethod(
      Map<String, String> properties, String path) {
    String encodedPath = path;
    try {
      encodedPath = URLEncoder.encode(path, "UTF8");
    } catch (UnsupportedEncodingException e) {
      // Nothing
    }
    return HttpUtils.createHttpMethod(
        getWiki().getSettings().getHostURL(true) + "/" + encodedPath,
        properties, false);
  }
}
