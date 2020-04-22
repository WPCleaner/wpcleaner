/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.http.hc5;

import java.io.IOException;
import java.util.Map;

import org.apache.hc.client5.http.classic.methods.HttpUriRequest;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.HttpStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.ResponseManager;
import org.wikipediacleaner.api.http.HttpServer;


/**
 * Manage interactions with the tool server.
 */
public class Hc5HttpServer implements HttpServer {

  /** Logs. */
  private static final Logger log = LoggerFactory.getLogger(Hc5HttpServer.class);

  /** HTTP Client. */
  private final CloseableHttpClient httpClient;

  /**
   * Base URL.
   */
  private final String baseUrl;

  /**
   * Maximum number of attempts.
   */
  private final static int MAX_ATTEMPTS = 3;

  /**
   * Create an HttpServer object.
   * 
   * @param httpClient HTTP client.
   * @param baseUrl Base URL of the server.
   */
  public Hc5HttpServer(CloseableHttpClient httpClient, String baseUrl) {
    this.httpClient = httpClient;
    this.baseUrl = baseUrl;
  }

  /**
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param properties Request properties.
   * @param manager Response manager.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void sendPost(
      String              path,
      Map<String, String> properties,
      ResponseManager     manager) throws APIException {

    int count = 0;
    int statusCode = HttpStatus.SC_SEE_OTHER;
    String url = baseUrl + path;
    while (count < MAX_ATTEMPTS) {

      // Wait if it's not the first attempt
      count++;
      if (count > 1) {
        waitBeforeRetrying(count);
      }

      // Perform the request
      try {
        final HttpUriRequest method = Hc5HttpUtils.createMethod(url,  properties, false);
        final Hc5HttpResponse response = httpClient.execute(method, new Hc5HttpResponseHandler());
        statusCode = response.status;
        if (statusCode == HttpStatus.SC_OK) {
          if (manager != null) {
            manager.manageResponse(response.inputStream);
          }
          return;
        }
      } catch (IOException e) {
        log.error("IOException (" + url + "): " + e.getMessage());
      }
    }
    throw new APIException("POST returned " + statusCode);
  }

  /**
   * Send a GET request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param manager Response manager.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void sendGet(
      String          path,
      ResponseManager manager) throws APIException {

    int count = 0;
    int statusCode = HttpStatus.SC_SEE_OTHER;
    String url = baseUrl + path;
    while (count < MAX_ATTEMPTS) {

      // Wait if it's not the first attempt
      count++;
      if (count > 1) {
        waitBeforeRetrying(count);
      }

      // Perform the request
      try {
        final HttpUriRequest method = Hc5HttpUtils.createMethod(url, null, true);
        final Hc5HttpResponse response = httpClient.execute(method, new Hc5HttpResponseHandler());
        statusCode = response.status;
        if (statusCode == HttpStatus.SC_OK) {
          if (manager != null) {
            manager.manageResponse(response.inputStream);
          }
          return;
        }
      } catch (IOException e) {
        log.error("IOException (" + url + "): " + e.getMessage());
      }
    }
    throw new APIException("GET returned " + statusCode);
  }

  /**
   * @return Base URL.
   */
  @Override
  public String getBaseUrl() {
    return baseUrl;
  }

  /**
   * Wait after a problem occurred.
   */
  private void waitBeforeRetrying(int count) {
    try {
      Thread.sleep(10000 * Math.min(count, 1));
    } catch (InterruptedException e) {
      log.warn("Waiting before retrying");
    }
  }
}
