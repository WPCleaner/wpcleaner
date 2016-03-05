/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * Base class MediaWiki API list query requests.
 */
public class ApiListRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for List.
   */
  public final static String PROPERTY_LIST = "list";

  /**
   * Property value for List / Abuse filters.
   */
  public final static String PROPERTY_LIST_ABUSEFILTERS = "abusefilters";

  /**
   * Property value for List / Abuse log.
   */
  public final static String PROPERTY_LIST_ABUSELOG = "abuselog";

  /**
   * Property value for List / Back links.
   */
  public final static String PROPERTY_LIST_BACKLINKS = "backlinks";

  /**
   * Property value for List / Category members.
   */
  public final static String PROPERTY_LIST_CATEGORYMEMBERS = "categorymembers";

  /**
   * Property value for List / Embedded in.
   */
  public final static String PROPERTY_LIST_EMBEDDEDIN = "embeddedin";

  /**
   * Property value for List / Pages with property.
   */
  public final static String PROPERTY_LIST_PAGESWITHPROP = "pageswithprop";

  /**
   * Property value for List / Protected titles.
   */
  public final static String PROPERTY_LIST_PROTECTEDTITLES = "protectedtitles";

  /**
   * Property value for List / Query page.
   */
  public final static String PROPERTY_LIST_QUERYPAGE = "querypage";

  /**
   * Property value for List / Random pages.
   */
  public final static String PROPERTY_LIST_RANDOM = "random";

  /**
   * Property value for List / Recent changes.
   */
  public final static String PROPERTY_LIST_RECENTCHANGES = "recentchanges";

  /**
   * Property value for List / Search.
   */
  public final static String PROPERTY_LIST_SEARCH = "search";

  /**
   * Property value for List / Users.
   */
  public final static String PROPERTY_LIST_USERS = "users";

  /**
   * Property value for List / Raw watch list.
   */
  public final static String PROPERTY_LIST_WATCHLISTRAW = "watchlistraw";

  // ==========================================================================
  // Wiki management
  // ==========================================================================

  /**
   * Base constructor.
   * 
   * @param wiki Wiki.
   */
  protected ApiListRequest(EnumWikipedia wiki) {
    super(wiki);
  }
}
