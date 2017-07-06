/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.rest;

import org.apache.commons.httpclient.HttpClient;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Base interface for MediaWiki REST API results.
 */
public interface RestApiResult {

  /**
   * @return Wiki on which requests are made.
   */
  public EnumWikipedia getWiki();

  /**
   * @return HTTP client for making requests.
   */
  public HttpClient getHttpClient();
}
