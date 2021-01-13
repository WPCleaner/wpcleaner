/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.http.hc3;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.HeadMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.wikipediacleaner.api.http.HttpUtils;


/**
 * Utilities class for HTTP requests.
 */
public class Hc3HttpUtils extends HttpUtils {

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
    try {
      if (canUseGetMethod) {
        return createHttpGetMethod(url, properties);
      }
      return createHttpPostMethod(url, properties);
    } catch (URIException e) {
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
   * @throws URIException Exception if the URL is not correct.
   */
  private static PostMethod createHttpPostMethod(
      String url,
      Map<String, String> properties) throws URIException {
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("POST " + url) : null;
    org.apache.commons.httpclient.URI uri = new org.apache.commons.httpclient.URI(url, false, "UTF8");
    PostMethod method = new PostMethod();
    method.setURI(uri);
    method.getParams().setSoTimeout(60000);
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
        first = fillDebugUrl(debugUrl, first, key, value);
      }
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
   * @throws URIException Exception if the URL is not correct.
   */
  private static GetMethod createHttpGetMethod(
      String url,
      Map<String, String> properties) throws URIException {

    // Initialize GET Method
    org.apache.commons.httpclient.URI uri = new org.apache.commons.httpclient.URI(url, false, "UTF8");
    GetMethod method = new GetMethod();
    method.setURI(uri);
    method.getParams().setSoTimeout(60000);
    method.getParams().setContentCharset("UTF-8");
    method.setRequestHeader("Accept-Encoding", "gzip");

    // Manager query string
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("GET  " + url) : null;
    List<NameValuePair> params = new ArrayList<>();
    if (properties != null) {
      boolean first = true;
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        params.add(new NameValuePair(key, value));
        first = fillDebugUrl(debugUrl, first, key, value);
      }
    }
    if (DEBUG_URL && (debugUrl != null)) {
      debugText(debugUrl.toString());
    }
    NameValuePair[] tmpParams = new NameValuePair[params.size()];
    method.setQueryString(params.toArray(tmpParams));

    return method;
  }

  /**
   * Create an HTTP HEAD Method.
   * 
   * @param url URL of the request.
   * @param properties Properties to drive the API.
   * @return HEAD Method
   * @throws URIException Exception if the URL is not correct.
   */
  public static HeadMethod createHttpHeadMethod(
      String url,
      Map<String, String> properties) throws URIException {

    // Initialize HEAD Method
    org.apache.commons.httpclient.URI uri = null;
    try {
      uri = new org.apache.commons.httpclient.URI(url, true, "UTF8");
    } catch (URIException e) {
      uri = new org.apache.commons.httpclient.URI(url, false, "UTF8");
    }
    HeadMethod method = new HeadMethod();
    method.setURI(uri);
    method.getParams().setSoTimeout(60000);
    method.getParams().setContentCharset("UTF-8");
    method.setRequestHeader("Accept-Encoding", "gzip");

    // Manager query string
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("HEAD " + url) : null;
    List<NameValuePair> params = new ArrayList<>();
    if (properties != null) {
      boolean first = true;
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        params.add(new NameValuePair(key, value));
        first = fillDebugUrl(debugUrl, first, key, value);
      }
    }
    if (DEBUG_URL && (debugUrl != null)) {
      debugText(debugUrl.toString());
    }
    NameValuePair[] tmpParams = new NameValuePair[params.size()];
    method.setQueryString(params.toArray(tmpParams));

    return method;
  }
}
