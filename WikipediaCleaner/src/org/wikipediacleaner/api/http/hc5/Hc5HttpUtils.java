/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.http.hc5;

import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpUriRequest;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.apache.hc.core5.net.URIBuilder;
import org.wikipediacleaner.api.http.HttpUtils;


/**
 * Utilities class for HttpComponents5 HTTP requests.
 */
public class Hc5HttpUtils extends HttpUtils {

  // ==========================================================================
  // HTTP methods for HTTP Components 5
  // ==========================================================================

  /**
   * Create an HttpMethod.
   * 
   * @param url URL of the request.
   * @param properties Properties to add to the request.
   * @param canUseGetMethod Flag indicating if a GET method can be used.
   * @return HttpMethod.
   */
  public static HttpUriRequest createMethod(
      String url,
      Map<String, String> properties,
      boolean canUseGetMethod) {
    try {
      if (canUseGetMethod) {
        return createHttpGetMethod(url, properties);
      }
      return createHttpPostMethod(url, properties);
    } catch (URISyntaxException e) {
      log.error("Invalid URL {}: {}", url, e.getMessage());
      return null;
    }
  }

  /**
   * Create an HTTP POST Method.
   * 
   * @param url URL of the request.
   * @param properties Properties to drive the API.
   * @return POST Method
   * @throws URISyntaxException Exception if the URL is not correct.
   */
  private static HttpPost createHttpPostMethod(
      String url,
      Map<String, String> properties) throws URISyntaxException {

    // Initialize POST method
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("POST " + url) : null;
    HttpPost method = new HttpPost(new URIBuilder(url, StandardCharsets.UTF_8).build());
    method.addHeader("Accept-Encoding", "gzip");

    // Manage query parameters
    if (properties != null) {
      boolean first = true;
      List<org.apache.hc.core5.http.NameValuePair> params = new ArrayList<>();
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        params.add(new BasicNameValuePair(key, value));
        first = fillDebugUrl(debugUrl, first, key, value);
      }
      method.setEntity(new UrlEncodedFormEntity(params));
    }

    if (DEBUG_URL && (debugUrl != null)) {
      debugText(debugUrl.toString());
    }
    return method;
  }

  /**
   * Create an HTTP GET Method.
   * 
   * @param url URL of the request.
   * @param properties Properties to drive the API.
   * @return GET Method
   * @throws URISyntaxException Exception if the URL is not correct.
   */
  private static HttpGet createHttpGetMethod(
      String url,
      Map<String, String> properties) throws URISyntaxException {

    // Initialize URI
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("GET  " + url) : null;
    URIBuilder uriBuilder = new URIBuilder(url, StandardCharsets.UTF_8);
    if (properties != null) {
      boolean first = true;
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        uriBuilder.addParameter(key, value);
        first = fillDebugUrl(debugUrl, first, key, value);
      }
    }

    // Initialize GET Method
    HttpGet method = new HttpGet(uriBuilder.build());
    method.addHeader("Accept-Encoding", "gzip");

    if (DEBUG_URL && (debugUrl != null)) {
      debugText(debugUrl.toString());
    }
    return method;
  }
}
