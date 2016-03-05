/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki random pages requests.
 */
public class ApiRandomPagesRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "rnlimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "rnnamespace";

  /**
   * Property for Redirect.
   */
  public final static String PROPERTY_REDIRECT = "rnredirect";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiRandomPagesResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiRandomPagesRequest(EnumWikipedia wiki, ApiRandomPagesResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of random pages.
   * 
   * @param count Maximum number of pages to get.
   * @param redirects True if redirect pages are requested.
   * @return List of random pages.
   */
  public List<Page> loadRandomList(int count, boolean redirects) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_RANDOM);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_LIMIT, Integer.toString(count));
    properties.put(PROPERTY_NAMESPACE, Integer.toString(Namespace.MAIN));
    if (redirects) {
      properties.put(PROPERTY_REDIRECT, "");
    }
    List<Page> list = new ArrayList<Page>();
    result.executeRandomList(properties, list);
    return list;
  }
}
