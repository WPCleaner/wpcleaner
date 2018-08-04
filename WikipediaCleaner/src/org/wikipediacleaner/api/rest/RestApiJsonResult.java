/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.rest;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;


/**
 * MediaWiki REST API JSON results.
 */
public abstract class RestApiJsonResult extends BasicRestApiResult {

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /** Flag for tracing JSON */
  private static boolean DEBUG_JSON = false;

  /** JSON factory */
  protected final static JsonFactory factory = new JsonFactory();

  /**
   * Update configuration.
   */
  public static void updateConfiguration() {
    Configuration config = Configuration.getConfiguration();
    DEBUG_JSON = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_API);
    DEBUG_JSON = true; // TODO
  }

  // ==========================================================================
  // JSON Results
  // ==========================================================================

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public RestApiJsonResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Send a request to MediaWiki REST API.
   * 
   * @param properties Properties defining the request.
   * @param path Path to REST API method.
   * @param param Parametere for REST API method.
   * @param maxTry Maximum number of tries.
   * @return Answer of MediaWiki API.
   * @throws APIException Exception thrown by the API.
   */
  protected JsonNode getRoot(
      Map<String, String> properties,
      String path, String param,
      int maxTry)
          throws APIException {
    int attempt = 0;
    for (;;) {
      JsonNode root = null;
      HttpMethod method = null;
      InputStream stream = null;
      try {
        // Executing HTTP method
        attempt++;
        method = createHttpMethod(properties, path, param);
        int statusCode = getHttpClient().executeMethod(method);

        // Accessing response
        stream = method.getResponseBodyAsStream();
        stream = new BufferedInputStream(stream);
        Header contentEncoding = method.getResponseHeader("Content-Encoding");
        if (contentEncoding != null) {
          if (contentEncoding.getValue().equals("gzip")) {
            stream = new GZIPInputStream(stream);
          }
        }

        // Read the response
        if (statusCode == HttpStatus.SC_OK){
          ObjectMapper mapper = new ObjectMapper(factory);
          root = mapper.readValue(stream, JsonNode.class);
          traceDocument(root);
          checkForError(root);
        } else {
          try {
            while (stream.read() >= 0) {
              //
            }
          } catch (IOException e) {
            //
          }
        }

        // Act depending on the status
        if (statusCode != HttpStatus.SC_OK) {
          String message = "URL access returned " + HttpStatus.getStatusText(statusCode);
          log.error(message);
          if (attempt > maxTry) {
            log.warn("Error. Maximum attempts count reached.");
            throw new APIException(message);
          }
          try {
            Thread.sleep(30000);
          } catch (InterruptedException e) {
            // Nothing
          }
        } else {
          return root;
        }
      } catch (IOException e) {
        String message = "IOException: " + e.getMessage();
        log.error(message);
        if (attempt > maxTry) {
          log.warn("Error. Maximum attempts count reached.");
          throw new APIException("Error accessing MediaWiki", e);
        }
        try {
          Thread.sleep(30000);
        } catch (InterruptedException e2) {
          // Nothing
        }
      } catch (APIException e) {
        if (!e.shouldRetry() || (attempt > e.getMaxRetry())) {
          throw e;
        }
        e.waitForRetry();
      } finally {
        if (stream != null) {
          try {
            stream.close();
          } catch (IOException e) {
            log.warn("Error closing stream");
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
      log.warn("Error. Trying again");
    }
  }

  /**
   * Check for errors reported by the API.
   * 
   * @param root Document root.
   * @throws APIException Exception thrown by the API.
   */
  protected void checkForError(JsonNode root) throws APIException {
    if (root == null) {
      return;
    }
    
    // Check for errors
    JsonNode error = root.path("error");
    if ((error != null) && !error.isMissingNode()) {
      String code = error.path("code").asText("?");
      String info = error.path("info").asText("?");
      String text = "Error reported: " + code + " - " + info;
      log.warn(text);
      throw new APIException(text, code);
    }

    // Check for warnings
    JsonNode warnings = root.path("warnings");
    if ((warnings != null) && !warnings.isMissingNode()) {
      log.warn("Warning reported: ");
      String query = warnings.path("query").asText();
      if (query != null) {
        log.warn(query);
      }
      String info = warnings.path("info").asText();
      if (info != null) {
        log.warn(info);
      }
    }
  }

  /**
   * Trace a document contents.
   * 
   * @param parser JSON parser.
   */
  private void traceDocument(JsonNode root) {
    if (DEBUG_JSON) {
      System.out.println("********** START OF DOCUMENT **********");
      if (root != null) {
        System.out.println(root.toString());
      }
      System.out.println("**********  END OF DOCUMENT  **********");
    }
  }
}
