/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.http;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;

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
  protected final static Logger log = LoggerFactory.getLogger("API");

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * Flag for tracing secret keys.
   */
  protected static boolean DEBUG_SECRET_KEYS = false;

  /**
   * Flag for tracing time.
   */
  protected static boolean DEBUG_TIME = false;

  /**
   * Flag for tracing URL.
   */
  protected static boolean DEBUG_URL = true;

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
  // Debug URL
  // ==========================================================================

  /**
   * Add a parameter to the debug URL.
   *  
   * @param debugUrl Current value of the debug URL.
   * @param first True if it's the first parameter.
   * @param key Name of the parameter.
   * @param value Value of the parameter.
   * @return True if it's still the first parameter.
   */
  protected static boolean fillDebugUrl(StringBuilder debugUrl, boolean first, String key, String value) {
    if (!DEBUG_URL || (debugUrl == null)) {
      return first;
    }
    if (!DEBUG_SECRET_KEYS && isSecretKey(key)) {
      return first;
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
    return false;
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
  protected static void debugText(String text) {
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
