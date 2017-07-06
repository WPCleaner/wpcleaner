/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.rest;

import java.util.HashMap;
import java.util.Map;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Base class for MediaWiki REST API requests.
 */
public abstract class RestApiRequest {

  // ==========================================================================
  // API management
  // ==========================================================================

  /**
   * Maximum number of attempts for a request.
   */
  public final static int MAX_ATTEMPTS = 2;

  // ==========================================================================
  // Wiki management
  // ==========================================================================

  private final EnumWikipedia wiki;

  /**
   * Base constructor.
   * 
   * @param wiki Wiki.
   */
  protected RestApiRequest(EnumWikipedia wiki) {
    this.wiki = wiki;
  }

  /**
   * @return Wiki.
   */
  protected EnumWikipedia getWiki() {
    return wiki;
  }

  // ==========================================================================
  // Request management
  // ==========================================================================

  /**
   * Initialize a set of properties.
   * 
   * @return Properties.
   */
  protected Map<String, String> getProperties() {
    Map<String, String> properties = new HashMap<String, String>();
    return properties;
  }
}
