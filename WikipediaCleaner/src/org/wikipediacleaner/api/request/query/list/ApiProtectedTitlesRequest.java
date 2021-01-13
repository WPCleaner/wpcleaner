/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * MediaWiki protected titles requests.
 */
public class ApiProtectedTitlesRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "ptdir";

  /**
   * Property value for Direction / Newer.
   */
  public final static String PROPERTY_DIR_NEWER = "newer";

  /**
   * Property value for Direction / Older.
   */
  public final static String PROPERTY_DIR_OLDER = "older";

  /**
   * Property for End.
   */
  public final static String PROPERTY_END = "ptend";

  /**
   * Property for Level.
   */
  public final static String PROPERTY_LEVEL = "ptlevel";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "ptlimit";

  /**
   * Property for Namespace.
   */
  public final static String PROPERTY_NAMESPACE = "ptnamespace";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "ptprop";

  /**
   * Property value for Properties / Comment.
   */
  public final static String PROPERTY_PROPERTIES_COMMENT = "comment";

  /**
   * Property value for Properties / Expiry.
   */
  public final static String PROPERTY_PROPERTIES_EXPIRY = "expiry";

  /**
   * Property value for Properties / LEVEL.
   */
  public final static String PROPERTY_PROPERTIES_LEVEL = "level";

  /**
   * Property value for Properties / Timestamp.
   */
  public final static String PROPERTY_PROPERTIES_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / User.
   */
  public final static String PROPERTY_PROPERTIES_USER = "user";

  /**
   * Property for Start.
   */
  public final static String PROPERTY_START = "ptnamespace";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiProtectedTitlesResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiProtectedTitlesRequest(EnumWikipedia wiki, ApiProtectedTitlesResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of protected titles.
   * 
   * @param namespaces List of name spaces to restrict result.
   * @param limit Flag indicating if the number of results should be limited.
   * @return List of protected titles.
   * @throws APIException Exception thrown by the API.
   */
  public List<Page> loadProtectedTitles(
      List<Integer> namespaces,
      boolean limit) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_PROTECTEDTITLES);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    if ((namespaces != null) && (namespaces.size() > 0)) {
      properties.put(PROPERTY_NAMESPACE, constructList(namespaces));
    }
    properties.put(PROPERTY_LEVEL, "sysop");
    properties.put(PROPERTY_PROPERTIES, PROPERTY_PROPERTIES_EXPIRY);
    List<Page> list = new ArrayList<>();
    int maxSize = getMaxSize(limit, ConfigurationValueInteger.MAX_PROTECTED_TITLES);
    while (result.executeProtectedTitles(properties, list) &&
           (list.size() < maxSize)) {
      //
    }
    Collections.sort(list);
    return list;
  }
}
