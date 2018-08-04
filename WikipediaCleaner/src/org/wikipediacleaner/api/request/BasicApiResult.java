/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.HttpUtils;
import org.wikipediacleaner.api.constants.ConnectionInformation;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.login.ApiLoginRequest;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * MediaWiki API XML results.
 */
public abstract class BasicApiResult implements ApiResult {

  /**
   * Logger.
   */
  protected final Logger log = LoggerFactory.getLogger(BasicApiResult.class);

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
  protected BasicApiResult(
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
   * @return HttpMethod.
   */
  protected HttpMethod createHttpMethod(
      Map<String, String> properties) {
    if (shouldSendIdentification()) {
      ConnectionInformation connection = wiki.getConnection();
      if (connection.getLgToken() != null) {
        properties.put(
            ApiLoginRequest.PROPERTY_TOKEN,
            connection.getLgToken());
      }
      if (connection.getLgUserName() != null) {
        properties.put(
            ApiLoginRequest.PROPERTY_USER_NAME,
            connection.getLgUserName());
      }
      if (connection.getLgUserId() != null) {
        properties.put(
            ApiLoginRequest.PROPERTY_USER_ID,
            connection.getLgUserId());
      }
    }
    boolean getMethod = canUseGetMethod(properties);
    Configuration config = Configuration.getConfiguration();
    boolean useHttps = !config.getBoolean(null, ConfigurationValueBoolean.FORCE_HTTP_API);
    return HttpUtils.createHttpMethod(
        getWiki().getSettings().getApiURL(useHttps),
        properties,
        getMethod);
  }

  /**
   * @return True if identification parameters should be sent.
   */
  protected boolean shouldSendIdentification() {
    return false;
  }

  /**
   * @param properties Properties to drive the API.
   * @return True if GET method can be used.
   */
  private boolean canUseGetMethod(Map<String, String> properties) {
    if (properties == null) {
      return false;
    }
    String action = properties.get("action");
    if (action == null) {
      return false;
    }
    if (ApiRequest.ACTION_QUERY.equals(action)) {
      return true;
    }
    return false;
  }
}
