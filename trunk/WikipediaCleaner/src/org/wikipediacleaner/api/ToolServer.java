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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * Manage interactions with the tool server.
 */
public class ToolServer {

  private final Log log = LogFactory.getLog(ToolServer.class);

  /**
   * HTTP Client.
   */
  private final HttpClient httpClient;

  /**
   * Create a ToolServer object.
   * 
   * @param httpClient HTTP client.
   */
  ToolServer(HttpClient httpClient) {
    this.httpClient = httpClient;
  }

  /**
   * Retrieve list of pages for a given error.
   * 
   * @param algorithm Algorithm.
   * @param errorLimit Maximum number of pages.
   * @param wiki Wiki.
   * @param errors List of errors.
   * @throws APIException
   */
  public void retrievePagesForError(
      final CheckErrorAlgorithm algorithm, int errorLimit,
      final EnumWikipedia wiki,
      final List<CheckError> errors) throws APIException {
    // Retrieving list of pages for the error number
    String code = wiki.getSettings().getCodeCheckWiki().replace("-", "_");
    Map<String, String> properties = new HashMap<String, String>();
    properties.put("id", algorithm.getErrorNumberString());
    properties.put("limit", Integer.toString(errorLimit));
    properties.put("offset", Integer.toString(0));
    properties.put("project", code);
    properties.put("view", "bots");
    ResponseManager manager = new ResponseManager() {
      
      public void manageResponse(InputStream stream)
          throws IOException, APIException {
        CheckError.addCheckError(
            errors, wiki,
            Integer.valueOf(algorithm.getErrorNumberString()), stream);
      }
    };
    sendPost(
        "~sk/cgi-bin/checkwiki/checkwiki.cgi", properties, manager);
  }

  /**
   * Mark a page as fixed.
   * 
   * @param page Page.
   * @param errorNumber Error number.
   * @return True if it has been done.
   */
  public boolean markPageAsFixed(Page page, String errorNumber) {
    try {
      Map<String, String> properties = new HashMap<String, String>();
      properties.put("id", Integer.toString(Integer.parseInt(errorNumber)));
      properties.put("pageid", Integer.toString(page.getPageId()));
      properties.put("project", page.getWikipedia().getSettings().getCodeCheckWiki());
      properties.put("view", "only");
      sendPost(
          "~sk/cgi-bin/checkwiki/checkwiki.cgi", properties, null);
    } catch (NumberFormatException e) {
      return false;
    } catch (APIException e) {
      return false;
    }
    return true;
  }

  /**
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param properties Request properties.
   * @param manager Response manager.
   * @throws APIException
   */
  private void sendPost(
      String              path,
      Map<String, String> properties,
      ResponseManager     manager) throws APIException {
    HttpMethod method = null;
    InputStream inputStream = null;
    try {
      String url = "http://toolserver.org/" + path;
      method = HttpUtils.createHttpMethod(url, properties, false);
      int statusCode = httpClient.executeMethod(method);
      inputStream = method.getResponseBodyAsStream();
      inputStream = new BufferedInputStream(inputStream);
      Header contentEncoding = method.getResponseHeader("Content-Encoding");
      if (contentEncoding != null) {
        if (contentEncoding.getValue().equals("gzip")) {
          inputStream = new GZIPInputStream(inputStream);
        }
      }
      if ((statusCode == HttpStatus.SC_OK) && (manager != null)) {
        manager.manageResponse(inputStream);
      }
      try {
        while (inputStream.read() >= 0) {
          //
        }
      } catch (IOException e) {
        //
      }
      if (statusCode != HttpStatus.SC_OK) {
        throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
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
    try {
      String url = "http://toolserver.org/" + path;
      method = HttpUtils.createHttpMethod(url, null, true);
      int statusCode = httpClient.executeMethod(method);
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
      if ((statusCode == HttpStatus.SC_OK) && (manager != null)) {
        manager.manageResponse(inputStream);
      }
      try {
        while (inputStream.read() >= 0) {
          //
        }
      } catch (IOException e) {
        //
      }
      if (statusCode != HttpStatus.SC_OK) {
        throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
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
}
