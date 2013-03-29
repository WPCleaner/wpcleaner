/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.request;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.HttpUtils;
import org.wikipediacleaner.api.constants.ConnectionInformation;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * MediaWiki API XML results.
 */
public abstract class BasicApiResult implements ApiResult {

  /**
   * Logger.
   */
  protected final Log log = LogFactory.getLog(BasicApiResult.class);

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
  public EnumWikipedia getWiki() {
    return wiki;
  }

  /**
   * @return HTTP client for making requests.
   */
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
    if (ApiRequest.ACTION_PURGE.equals(action) ||
        ApiRequest.ACTION_QUERY.equals(action)) {
      return true;
    }
    return false;
  }
}
