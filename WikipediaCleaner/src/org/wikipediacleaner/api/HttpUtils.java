/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.request.login.ApiLoginRequest;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Utilities class for HTTP requests.
 */
public class HttpUtils {

  /** Logger */
  private final static Logger log = LoggerFactory.getLogger("API");

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
    List<NameValuePair> params = new ArrayList<NameValuePair>();
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
    org.apache.commons.httpclient.URI uri = new org.apache.commons.httpclient.URI(url, false, "UTF8");
    HeadMethod method = new HeadMethod();
    method.setURI(uri);
    method.getParams().setSoTimeout(60000);
    method.getParams().setContentCharset("UTF-8");
    method.setRequestHeader("Accept-Encoding", "gzip");

    // Manager query string
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("HEAD " + url) : null;
    List<NameValuePair> params = new ArrayList<NameValuePair>();
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
   * Add a parameter to the debug URL.
   *  
   * @param debugUrl Current value of the debug URL.
   * @param first True if it's the first parameter.
   * @param key Name of the parameter.
   * @param value Value of the parameter.
   */
  private static boolean fillDebugUrl(StringBuilder debugUrl, boolean first, String key, String value) {
    if (!DEBUG_URL || (debugUrl == null)) {
      return false;
    }
    if (!DEBUG_SECRET_KEYS && isSecretKey(key)) {
      return false;
    }
    int start = 0;
    while ((start < value.length()) && Character.isWhitespace(value.charAt(start))) {
      start++;
    }
    if (value.indexOf('\n', start) > 0) {
      value = value.substring(start, value.indexOf('\n', start)) + "...";
    }
    debugUrl.append(first ? '?' : '&');
    debugUrl.append(key);
    debugUrl.append('=');
    debugUrl.append(isSecretKey(key) ? "XXXXX" : value);
    return true;
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
      log.info("" + System.currentTimeMillis() + ": " + text);
    } else {
      log.info(text);
    }
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  private static Charset utf8Charset = null;
  private static Charset iso88591Charset = null;

  static {
    utf8Charset = Charset.forName("UTF8");
    iso88591Charset = Charset.forName("ISO-8859-1");
  }

  /**
   * Append bytes of a String to a buffer.
   * 
   * @param buf Byte buffer.
   * @param data String.
   * @throws UnsupportedEncodingException
   */
  private static void appendBytes(ByteArrayOutputStream buf, String data) throws UnsupportedEncodingException {
    byte[] b = data.getBytes("UTF8");
    buf.write(b, 0, b.length);
  }

  /**
   * Parse an encoded string into a byte array.
   * 
   * @param segment String
   * @return Byte array.
   * @throws UnsupportedEncodingException
   */
  private static byte[] parseEncodedString(String segment) throws UnsupportedEncodingException {
    ByteArrayOutputStream buf = new ByteArrayOutputStream(segment.length());
    int last = 0;
    int index = 0;
    while (index < segment.length()) {
      if (segment.charAt(index) == '%') {
        appendBytes(buf, segment.substring(last, index));
        if ((index + 2 < segment.length()) &&
            ("ABCDEFabcdef0123456789".indexOf(segment.charAt(index + 1)) >= 0) &&
            ("ABCDEFabcdef0123456789".indexOf(segment.charAt(index + 2)) >= 0)) {
          buf.write((byte) Integer.parseInt(segment.substring(index + 1, index + 3), 16));
          index += 3;
        } else if ((index + 1 < segment.length()) &&
                   (segment.charAt(index + 1) == '%')) {
          buf.write((byte) '%');
          index += 2;
        } else {
          buf.write((byte) '%');
          index++;
        }
        last = index;
      } else {
        index++;
      }
    }
    appendBytes(buf, segment.substring(last));
    return buf.toByteArray();
  }

  /**
   * Parse an encoded string, trying several characters sets.
   * 
   * @param segment String to parse.
   * @param encodings Characters sets.
   * @return Decoded string.
   */
  public static String parseEncodedString(String segment, Charset... encodings) {
    if ((segment == null) || (segment.indexOf('%') < 0)) {
      return segment;
    }
    try {
      byte[] data = parseEncodedString(segment);
      for (Charset encoding : encodings) {
        try {
          if (encoding != null) {
            return encoding.newDecoder().
                onMalformedInput(CodingErrorAction.REPORT).
                decode(ByteBuffer.wrap(data)).toString();
          }
        } catch (CharacterCodingException e) {
          // Incorrect encoding, try next one
        }
      }
    } catch (UnsupportedEncodingException e) {
      // Nothing to do
    }
    return segment;
  }

  /**
   * Find if a given URL is for an article.
   * 
   * @param url URL.
   * @param base Base URL.
   * @return Article name or null if it doesn't match an article.
   */
  public static String getArticleFromUrl(String url, String base) {
    if ((url == null) || (base == null)) {
      return null;
    }

    // Cleanup URL
    while (url.endsWith("|")) {
      url = url.substring(0, url.length() - 1);
    }

    // Create URI
    URI uri = null;
    try {
      uri = new URI(url);
    } catch (URISyntaxException e) {
      url = url.replaceAll("\\&\\#x20\\;", "_");
      try {
        uri = new URI(url);
      } catch (URISyntaxException e2) {
        return null;
      }
    }

    // Various checks
    if (!uri.isAbsolute() || uri.isOpaque()) {
      return null;
    }

    // Check scheme
    String scheme = uri.getScheme();
    if (scheme == null) {
      return null;
    }
    if (!scheme.equalsIgnoreCase("http") &&
        !scheme.equalsIgnoreCase("https")) {
      return null;
    }

    // Build decoded parts
    StringBuilder details = new StringBuilder();
    details.append("//");
    details.append(uri.getAuthority());
    details.append(parseEncodedString(uri.getRawPath(), utf8Charset, iso88591Charset));
    if (uri.getQuery() != null) {
      details.append("?");
      details.append(uri.getQuery());
    }
    if (uri.getFragment() != null) {
      details.append("#");
      details.append(uri.getFragment());
    }

    // Check that URL starts correctly
    int paramIndex = base.indexOf("$1");
    if (paramIndex < 0) {
      return null;
    }
    String detailsStr = details.toString();
    if (!detailsStr.startsWith(base.substring(0, paramIndex))) {
      return null;
    }

    // Check that URL ends correctly
    String result = null;
    if (paramIndex + 2 >= base.length()) {
      result = detailsStr.substring(paramIndex);
    } else if (!detailsStr.endsWith(base.substring(paramIndex + 2))) {
      return null;
    } else {
      result = detailsStr.substring(paramIndex, details.length() - base.length() + 2 + paramIndex);
    }
    if (result != null) {
      result = result.replaceAll("\\_", " ");
      if (result.endsWith("/")) {
        result = result.substring(0, result.length() - 1);
      }
    }
    return result;
  }
}
