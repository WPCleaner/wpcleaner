/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
   * Check Wiki project.
   */
  private static CheckWiki checkWiki;

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

    // Initialize Check Wiki project
    checkWiki = new CheckWiki(toolServer);
  }

  /**
   * @return MediaWiki API implementation.
   */
  public static API getAPI() {
    return api;
  }

  /**
   * @return Access to Check Wiki project.
   */
  public static CheckWiki getCheckWiki() {
    return checkWiki;
  }

  /**
   * @return Access to the tool server.
   */
  static ToolServer getToolServer() {
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
