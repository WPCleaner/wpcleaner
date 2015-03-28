/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Base class for MediaWiki API meta requests.
 */
public class ApiMetaRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Meta.
   */
  public final static String PROPERTY_META = "meta";

  /**
   * Property for Meta / All messages.
   */
  public final static String PROPERTY_META_ALLMESSAGES = "allmessages";

  /**
   * Property for Meta / Site info.
   */
  public final static String PROPERTY_META_SITEINFO = "siteinfo";

  // ==========================================================================
  // Wiki management
  // ==========================================================================

  /**
   * Base constructor.
   * 
   * @param wiki Wiki.
   */
  protected ApiMetaRequest(EnumWikipedia wiki) {
    super(wiki);
  }
}
