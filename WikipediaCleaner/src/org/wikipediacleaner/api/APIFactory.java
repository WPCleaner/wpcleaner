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
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.http.HttpServer;
import org.wikipediacleaner.api.http.hc3.Hc3HttpServer;
import org.wikipediacleaner.api.impl.MediaWikiAPI;
import org.wikipediacleaner.Version;


/**
 * Factory for API access. 
 */
public class APIFactory {

  /** MediaWiki API */
  private static API api;

  /** MediaWiki REST API */
  private static MediaWikiRESTAPI restApi;

  /** Check Wiki project */
  private static CheckWiki checkWiki;

  // Initialize static members
  static {

    // Initialize MediaWiki API
    HttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
    HttpClient httpClient = createHttpClient(connectionManager);
    httpClient.getParams().setParameter("http.protocol.single-cookie-header", Boolean.TRUE);
    api = new MediaWikiAPI(httpClient);

    // Initialize MediaWiki REST API
    connectionManager = new MultiThreadedHttpConnectionManager();
    httpClient = createHttpClient(connectionManager);
    httpClient.getParams().setParameter("http.protocol.single-cookie-header", Boolean.TRUE);
    restApi = new MediaWikiRESTAPI(httpClient);

    // Initialize WMF Labs access
    /* HttpComponents 5
    PoolingHttpClientConnectionManager connManager = new PoolingHttpClientConnectionManager();
    connManager.setMaxTotal(200);
    connManager.setDefaultMaxPerRoute(50);
    HttpServer labs = new Hc5HttpServer(createHttpClient(connManager), "https://checkwiki.toolforge.org/"); */
    connectionManager = new MultiThreadedHttpConnectionManager();
    httpClient = createHttpClient(connectionManager);
    HttpServer labs = new Hc3HttpServer(httpClient, "https://checkwiki.toolforge.org/");

    // Initialize Check Wiki project
    checkWiki = new CheckWiki(labs, "");
  }

  /**
   * @return MediaWiki API implementation.
   */
  public static API getAPI() {
    return api;
  }

  /**
   * @return MediaWiki REST API implementation.
   */
  public static MediaWikiRESTAPI getRESTAPI() {
    return restApi;
  }

  /**
   * @return Access to Check Wiki project.
   */
  public static CheckWiki getCheckWiki() {
    return checkWiki;
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
        "WPCleaner/" + Version.VERSION + " (+http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation)");
    return client;
  }
  
  /**
   * Create an HTTP client.
   * 
   * @param manager Connection manager.
   * @return HTTP client.
   */
  /*private static CloseableHttpClient createHttpClient(HttpClientConnectionManager manager) {
    return HttpClients.custom().
        setConnectionManager(manager).
        setUserAgent("WPCleaner (+http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation)").
        build();
  }*/
}
