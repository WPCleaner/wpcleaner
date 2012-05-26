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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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
   * Flag for tracing time.
   */
  public static boolean DEBUG_TIME = false;

  /**
   * Flag for tracing URL.
   */
  public static boolean DEBUG_URL = true;

  /**
   * Flag for tracing XML.
   */
  public static boolean DEBUG_XML = false;

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
    if (canUseGetMethod(properties)) {
      return createHttpGetMethod(properties);
    }
    return createHttpPostMethod(properties);
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

  /**
   * Create an HTTP POST Method.
   * 
   * @param properties Properties to drive the API.
   * @return POST Method
   */
  private PostMethod createHttpPostMethod(
      Map<String, String> properties) {
    String url = getWiki().getSettings().getApiURL();
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("POST " + url) : null;
    PostMethod method = new PostMethod(url);
    method.getParams().setContentCharset("UTF-8");
    method.setRequestHeader("Accept-Encoding", "gzip");
    if (properties != null) {
      boolean first = true;
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        method.addParameter(key, value);
        if (DEBUG_URL && (debugUrl != null)) {
          int start = 0;
          while ((start < value.length()) && Character.isWhitespace(value.charAt(start))) {
            start++;
          }
          if (value.indexOf('\n', start) > 0) {
            value = value.substring(start, value.indexOf('\n', start)) + "...";
          }
          debugUrl.append(
              (first ? "?" : "&") +
              key + "=" +
              (ApiLoginRequest.PROPERTY_PASSWORD.equals(key) ? "XXXXX" : value));
          first = false;
        }
      }
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
      }
    }
    if (connection.getLgToken() != null) {
      method.addParameter(
          ApiLoginRequest.PROPERTY_TOKEN,
          connection.getLgToken());
    }
    if (connection.getLgUserName() != null) {
      method.addParameter(
          ApiLoginRequest.PROPERTY_USER_NAME,
          connection.getLgUserName());
    }
    if (connection.getLgUserId() != null) {
      method.addParameter(
          ApiLoginRequest.PROPERTY_USER_ID,
          connection.getLgUserId());
    }
    return method;
  }

  /**
   * Create an HTTP GET Method.
   * 
   * @param properties Properties to drive the API.
   * @return GET Method
   */
  private GetMethod createHttpGetMethod(
      Map<String, String> properties) {

    // Initialize GET Method
    String url = getWiki().getSettings().getApiURL();
    GetMethod method = new GetMethod(url);
    method.getParams().setContentCharset("UTF-8");
    method.setRequestHeader("Accept-Encoding", "gzip");

    // Manager query string
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("GET  " + url) : null;
    List<NameValuePair> params = new ArrayList<NameValuePair>();
    if (properties != null) {
      boolean first = true;
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        params.add(new NameValuePair(key, value));
        if (DEBUG_URL && (debugUrl != null)) {
          int start = 0;
          while ((start < value.length()) && Character.isWhitespace(value.charAt(start))) {
            start++;
          }
          if (value.indexOf('\n', start) > 0) {
            value = value.substring(start, value.indexOf('\n', start)) + "...";
          }
          debugUrl.append(
              (first ? "?" : "&") +
              key + "=" +
              (ApiLoginRequest.PROPERTY_PASSWORD.equals(key) ? "XXXXX" : value));
        }
        first = false;
      }
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
      }
    }
    if (connection.getLgToken() != null) {
      params.add(new NameValuePair(
          ApiLoginRequest.PROPERTY_TOKEN,
          connection.getLgToken()));
    }
    if (connection.getLgUserName() != null) {
      params.add(new NameValuePair(
          ApiLoginRequest.PROPERTY_USER_NAME,
          connection.getLgUserName()));
    }
    if (connection.getLgUserId() != null) {
      params.add(new NameValuePair(
          ApiLoginRequest.PROPERTY_USER_ID,
          connection.getLgUserId()));
    }
    NameValuePair[] tmpParams = new NameValuePair[params.size()];
    method.setQueryString(params.toArray(tmpParams));

    return method;
  }

  /**
   * @param text Text to add to debug.
   */
  private void debugText(String text) {
    if (DEBUG_TIME) {
      System.out.println("" + System.currentTimeMillis() + ": " + text);
    } else {
      System.out.println(text);
    }
  }
}
