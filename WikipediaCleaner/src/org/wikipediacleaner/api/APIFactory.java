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
   * HTTP connection manager.
   */
  private static HttpConnectionManager connectionManger;

  /**
   * MediaWiki API.
   */
  private static API api;

  static {
    connectionManger = new MultiThreadedHttpConnectionManager();
    api = new MediaWikiAPI(connectionManger);
  }

  /**
   * @return MediaWiki API implementation.
   */
  public static API getAPI() {
    return api;
  }

  /**
   * Create an HTTP connection.
   * 
   * @return A HTTP connection.
   */
  public static HttpClient createHttpClient(HttpConnectionManager manager) {
    HttpClient client = new HttpClient(manager);
    client.getParams().setParameter(
        HttpMethodParams.USER_AGENT,
        "WPCleaner (+http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation)");
    return client;
  }
}
