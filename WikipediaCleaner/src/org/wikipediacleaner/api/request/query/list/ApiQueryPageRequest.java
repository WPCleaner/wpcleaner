/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumQueryPage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * MediaWiki query page requests.
 */
public class ApiQueryPageRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "qplimit";

  /**
   * Property for special page.
   */
  public final static String PROPERTY_PAGE = "qppage";

  /**
   * Property for Offset.
   */
  public final static String PROPERTY_OFFSET = "qpoffset";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiQueryPageResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiQueryPageRequest(EnumWikipedia wiki, ApiQueryPageResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load special list of pages.
   * 
   * @param query Type of list.
   * @return Special list of pages.
   * @throws APIException Exception thrown by the API.
   */
  public List<Page> loadQueryPage(
      EnumQueryPage query) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_QUERYPAGE);
    properties.put(PROPERTY_PAGE, query.getCode());
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    Set<Page> set = new HashSet<>();
    int maxSize = getMaxSize(true, ConfigurationValueInteger.MAX_QUERY_PAGE);
    while (result.executeQueryPage(properties, set) &&
           (set.size() < maxSize)) {
      //
    }
    List<Page> list = new ArrayList<>(set);
    Collections.sort(list);
    return list;
  }
}
