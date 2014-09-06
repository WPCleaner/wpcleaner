/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * Manage interactions with the tool server.
 */
public class HttpServer {

  /**
   * Logs.
   */
  private final Log log = LogFactory.getLog(HttpServer.class);

  /**
   * HTTP Client.
   */
  private final HttpClient httpClient;

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
   */
  HttpServer(HttpClient httpClient, String baseUrl) {
    this.httpClient = httpClient;
    this.baseUrl = baseUrl;
  }

  /**
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param properties Request properties.
   * @param manager Response manager.
   * @throws APIException
   */
  public void sendPost(
      String              path,
      Map<String, String> properties,
      ResponseManager     manager) throws APIException {
    HttpMethod method = null;
    InputStream inputStream = null;
    int statusCode = HttpStatus.SC_SEE_OTHER;
    int count = 0;
    while ((statusCode != HttpStatus.SC_OK) &&
           (count < MAX_ATTEMPTS)) {
      if (count > 0) {
        try {
          Thread.sleep(2000 * count);
        } catch (InterruptedException e) {
          // Nothing
        }
      }
      count++;
      try {
        String url = baseUrl + path;
        method = HttpUtils.createHttpMethod(url, properties, false);
        statusCode = httpClient.executeMethod(method);
        inputStream = method.getResponseBodyAsStream();
        inputStream = new BufferedInputStream(inputStream);
        Header contentEncoding = method.getResponseHeader("Content-Encoding");
        if (contentEncoding != null) {
          if (contentEncoding.getValue().equals("gzip")) {
            inputStream = new GZIPInputStream(inputStream);
          }
        }
        if (statusCode == HttpStatus.SC_OK) {
          if (manager != null) {
            manager.manageResponse(inputStream);
          }
        } else {
          log.warn("Error accessing url: " + statusCode + "-" + HttpStatus.getStatusText(statusCode));
          waitBeforeRetrying(count);
        }
        try {
          while (inputStream.read() >= 0) {
            //
          }
        } catch (IOException e) {
          //
        }
      } catch (HttpException e) {
        throw new APIException("HttpException: " + e.getMessage());
      } catch (IOException e) {
        throw new APIException("IOException: " + e.getMessage());
      } finally {
        if (inputStream != null) {
          try {
            inputStream.close();
          } catch (IOException e) {
            log.warn("Error closing stream: " + e.getMessage());
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
    }
    if (statusCode != HttpStatus.SC_OK) {
      throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
    }
  }

  /**
   * Send a GET request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param manager Response manager.
   * @throws APIException
   */
  public void sendGet(
      String          path,
      ResponseManager manager) throws APIException {
    HttpMethod method = null;
    InputStream inputStream = null;
    int statusCode = HttpStatus.SC_SEE_OTHER;
    int count = 0;
    while ((statusCode != HttpStatus.SC_OK) &&
           (count < MAX_ATTEMPTS)) {
      if (count > 0) {
        try {
          Thread.sleep(2000 * count);
        } catch (InterruptedException e) {
          // Nothing
        }
      }
      count++;
      try {
        String url = baseUrl + path;
        method = HttpUtils.createHttpMethod(url, null, true);
        statusCode = httpClient.executeMethod(method);
        if (statusCode == HttpStatus.SC_NOT_FOUND) {
          return;
        }
        inputStream = method.getResponseBodyAsStream();
        inputStream = new BufferedInputStream(inputStream);
        Header contentEncoding = method.getResponseHeader("Content-Encoding");
        if (contentEncoding != null) {
          if (contentEncoding.getValue().equals("gzip")) {
            inputStream = new GZIPInputStream(inputStream);
          }
        }
        if (statusCode == HttpStatus.SC_OK) {
          if (manager != null) {
            manager.manageResponse(inputStream);
          }
        } else {
          log.warn("Error accessing url: " + statusCode + "-" + HttpStatus.getStatusText(statusCode));
          waitBeforeRetrying(count);
        }
        try {
          while (inputStream.read() >= 0) {
            //
          }
        } catch (IOException e) {
          //
        }
      } catch (HttpException e) {
        throw new APIException("HttpException: " + e.getMessage());
      } catch (IOException e) {
        throw new APIException("IOException: " + e.getMessage());
      } finally {
        if (inputStream != null) {
          try {
            inputStream.close();
          } catch (IOException e) {
            log.warn("Error closing stream: " + e.getMessage());
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
    }
    if (statusCode != HttpStatus.SC_OK) {
      throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
    }
  }

  /**
   * @return Base URL.
   */
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
