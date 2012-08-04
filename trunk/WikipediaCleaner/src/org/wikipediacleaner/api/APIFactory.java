/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.wikipediacleaner.api.impl.MediaWikiAPI;


/**
 * Factory for API access. 
 */
public class APIFactory {

  /**
   * MediaWiki API.
   */
  private static API api;

  /**
   * Access to the tool server.
   */
  private static ToolServer toolServer;

  // Initialize static members
  static {

    // Initialize MediaWiki API
    HttpConnectionManager connectionManger = new MultiThreadedHttpConnectionManager();
    HttpClient httpClient = createHttpClient(connectionManger);
    api = new MediaWikiAPI(httpClient);

    // Initialize ToolServer access
    connectionManger = new MultiThreadedHttpConnectionManager();
    httpClient = createHttpClient(connectionManger);
    toolServer = new ToolServer(httpClient);
  }

  /**
   * @return MediaWiki API implementation.
   */
  public static API getAPI() {
    return api;
  }

  public static ToolServer getToolServer() {
    return toolServer;
  }

  /**
   * Create an HTTP connection.
   * 
   * @return A HTTP connection.
   */
  private static HttpClient createHttpClient(HttpConnectionManager manager) {
    HttpClient client = new HttpClient(manager);
    client.getParams().setParameter(
        HttpMethodParams.USER_AGENT,
        "WPCleaner (+http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation)");
    return client;
  }
}
