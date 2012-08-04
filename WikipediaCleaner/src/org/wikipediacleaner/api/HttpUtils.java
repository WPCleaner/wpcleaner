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

package org.wikipediacleaner.api;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.wikipediacleaner.api.request.ApiLoginRequest;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Utilities class for HTTP requests.
 */
public class HttpUtils {

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * Flag for tracing time.
   */
  private static boolean DEBUG_TIME = false;

  /**
   * Flag for tracing URL.
   */
  private static boolean DEBUG_URL = true;

  /**
   * Update configuration.
   */
  public static void updateConfiguration() {
    Configuration config = Configuration.getConfiguration();
    DEBUG_TIME = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_TIME);
    DEBUG_URL = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_URL);
  }

  // ==========================================================================
  // HTTP methods
  // ==========================================================================

  /**
   * Create an HttpMethod.
   * 
   * @param url URL of the request.
   * @param properties Properties to add to the request.
   * @param canUseGetMethod Flag indicating if a GET method can be used.
   * @return HttpMethod.
   */
  public static HttpMethod createHttpMethod(
      String url,
      Map<String, String> properties,
      boolean canUseGetMethod) {
    if (canUseGetMethod) {
      return createHttpGetMethod(url, properties);
    }
    return createHttpPostMethod(url, properties);
  }

  /**
   * Create an HTTP POST Method.
   * 
   * @param url URL of the request.
   * @param properties Properties to drive the API.
   * @return POST Method
   */
  private static PostMethod createHttpPostMethod(
      String url,
      Map<String, String> properties) {
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
              (isSecretKey(key) ? "XXXXX" : value));
          first = false;
        }
      }
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
      }
    }
    return method;
  }

  /**
   * Create an HTTP GET Method.
   * 
   * @param url URL of the request.
   * @param properties Properties to drive the API.
   * @return GET Method
   */
  private static GetMethod createHttpGetMethod(
      String url,
      Map<String, String> properties) {

    // Initialize GET Method
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
              (isSecretKey(key) ? "XXXXX" : value));
        }
        first = false;
      }
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
      }
    }
    NameValuePair[] tmpParams = new NameValuePair[params.size()];
    method.setQueryString(params.toArray(tmpParams));

    return method;
  }

  /**
   * @param key Key.
   * @return True if the value for the key should be kept secret.
   */
  private static boolean isSecretKey(String key) {
    boolean result = false;
    result |= ApiLoginRequest.PROPERTY_PASSWORD.equals(key);
    result |= ApiLoginRequest.PROPERTY_TOKEN.equals(key);
    result |= ApiLoginRequest.PROPERTY_USER_NAME.equals(key);
    result |= ApiLoginRequest.PROPERTY_USER_ID.equals(key);
    return result;
  }

  /**
   * Debug text.
   * 
   * @param text Text to add to debug.
   */
  private static void debugText(String text) {
    if (DEBUG_TIME) {
      System.out.println("" + System.currentTimeMillis() + ": " + text);
    } else {
      System.out.println(text);
    }
  }
}
