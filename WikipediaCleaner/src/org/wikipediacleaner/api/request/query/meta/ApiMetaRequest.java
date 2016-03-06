/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * Base class for MediaWiki API meta requests.
 */
public class ApiMetaRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /** Property for Meta */
  public final static String PROPERTY_META = "meta";

  /** Property for Meta / All messages */
  public final static String PROPERTY_META_ALLMESSAGES = "allmessages";

  /** Property for Meta / Site info */
  public final static String PROPERTY_META_SITEINFO = "siteinfo";

  /** Property for Meta / Tokens */
  public final static String PROPERTY_META_TOKENS = "tokens";

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
