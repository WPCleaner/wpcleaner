/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
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
   * Flag for tracing secret keys.
   */
  private static boolean DEBUG_SECRET_KEYS = false;

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
        if (DEBUG_URL &&
            (debugUrl != null) &&
            (DEBUG_SECRET_KEYS || !isSecretKey(key))) {
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
        if (DEBUG_URL &&
            (debugUrl != null) &&
            (DEBUG_SECRET_KEYS || !isSecretKey(key))) {
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

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @param url URL
   * @return Decoded URL
   */
  public static String decodeUrl(String url) {

    // Basic cases
    if ((url == null) ||
        (url.length() == 0) ||
        (url.contains("&"))) {
      return null;
    }
    url = url.replaceAll("\\_", " ");
    if (url.indexOf('%') < 0) {
      return url;
    }

    // URL decoding
    try {
      byte[] original = url.getBytes("UTF8");
      byte[] converted = new byte[original.length];
      int currentOriginal = 0;
      int currentConverted = 0;
      while (currentOriginal < original.length) {
        if ((original[currentOriginal] == '%') && (currentOriginal + 2 < original.length)) {
          byte[] hexa = new byte[2];
          hexa[0] = original[currentOriginal + 1];
          hexa[1] = original[currentOriginal + 2];
          String hexaString = new String(hexa, "UTF8");
          converted[currentConverted] = Integer.valueOf(hexaString, 16).byteValue();
          currentOriginal += 3;
          currentConverted++;
        } else {
          converted[currentConverted] = original[currentOriginal];
          currentOriginal++;
          currentConverted++;
        }
      }
      converted = Arrays.copyOf(converted, currentConverted);
      url = new String(converted, "UTF8");
    } catch (UnsupportedEncodingException e) {
      // Nothing to do
    } catch (NumberFormatException e) {
      // Nothing to do
    }
    return url;
  }
}