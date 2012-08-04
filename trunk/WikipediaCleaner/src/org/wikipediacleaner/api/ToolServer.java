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
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;


/**
 * Manage interactions with the tool server.
 */
public class ToolServer {

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
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param properties Request properties.
   * @param stream Flag indicating if the stream is needed.
   * @return Answer.
   * @throws APIException
   */
  public InputStream sendPost(
      String              path,
      Map<String, String> properties,
      boolean             stream) throws APIException {
    try {
      String url = "http://toolserver.org/" + path;
      HttpMethod method = HttpUtils.createHttpMethod(url, properties, false);
      int statusCode = httpClient.executeMethod(method);
      if (statusCode != HttpStatus.SC_OK) {
        throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
      }
      if (!stream) {
        return null;
      }
      InputStream inputStream = method.getResponseBodyAsStream();
      inputStream = new BufferedInputStream(inputStream);
      Header contentEncoding = method.getResponseHeader("Content-Encoding");
      if (contentEncoding != null) {
        if (contentEncoding.getValue().equals("gzip")) {
          inputStream = new GZIPInputStream(inputStream);
        }
      }
      return inputStream;
    } catch (HttpException e) {
      throw new APIException("HttpException: " + e.getMessage());
    } catch (IOException e) {
      throw new APIException("IOException: " + e.getMessage());
    }
  }

  /**
   * Send a GET request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param stream Flag indicating if the stream is needed.
   * @return Answer.
   * @throws APIException
   */
  public InputStream sendGet(
      String          path,
      boolean         stream) throws APIException {
    try {
      String url = "http://toolserver.org/" + path;
      HttpMethod method = HttpUtils.createHttpMethod(url, null, true);
      int statusCode = httpClient.executeMethod(method);
      if (statusCode == HttpStatus.SC_NOT_FOUND) {
        return null;
      }
      if (statusCode != HttpStatus.SC_OK) {
        throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
      }
      if (!stream) {
        return null;
      }
      InputStream inputStream = method.getResponseBodyAsStream();
      inputStream = new BufferedInputStream(inputStream);
      Header contentEncoding = method.getResponseHeader("Content-Encoding");
      if (contentEncoding != null) {
        if (contentEncoding.getValue().equals("gzip")) {
          inputStream = new GZIPInputStream(inputStream);
        }
      }
      return inputStream;
    } catch (HttpException e) {
      throw new APIException("HttpException: " + e.getMessage());
    } catch (IOException e) {
      throw new APIException("IOException: " + e.getMessage());
    }
  }
}
