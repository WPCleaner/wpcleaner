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
import org.wikipediacleaner.api.constants.EnumWikipedia;


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
   * Connection information.
   */
  private final ConnectionInformation connection;

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  protected BasicApiResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    this.wiki = wiki;
    this.httpClient = httpClient;
    this.connection = connection;
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

  /**
   * @return Connection information.
   */
  public ConnectionInformation getConnectionInformation() {
    return connection;
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
    return HttpUtils.createHttpMethod(
        getWiki().getSettings().getApiURL(),
        properties,
        canUseGetMethod(properties));
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
    // TODO: Enable this again when API bug 36839 is fixed.
    //if (ACTION_API_QUERY.equals(action)) {
    //  return true;
    //}
    return false;
  }
}
