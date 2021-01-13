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
 * MediaWiki embedded in requests.
 */
public class ApiEmbeddedInRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Filter redirection.
   */
  public final static String PROPERTY_FILTERREDIR = "eifilterredir";

  /**
   * Property value for Filter redirection / All.
   */
  public final static String PROPERTY_FILTERREDIR_ALL = "all";

  /**
   * Property value for Filter redirection / Non redirects.
   */
  public final static String PROPERTY_FILTERREDIR_NON_REDIRECTS = "nonredirects";

  /**
   * Property value for Filter redirection / Redirects.
   */
  public final static String PROPERTY_FILTERREDIR_REDIRECTS = "redirects";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "eilimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "einamespace";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "eititle";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiEmbeddedInResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiEmbeddedInRequest(EnumWikipedia wiki, ApiEmbeddedInResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of pages embedding a page.
   * 
   * @param page Page for list of embedding pages is requested.
   * @param namespaces List of name spaces to restrict result.
   * @param limit Flag indicating if the number of results should be limited.
   * @throws APIException Exception thrown by the API.
   */
  public void loadEmbeddedIn(
      Page page, List<Integer> namespaces,
      boolean limit) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_LIST, PROPERTY_LIST_EMBEDDEDIN);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    if ((namespaces != null) && (namespaces.size() > 0)) {
      properties.put(PROPERTY_NAMESPACE, constructList(namespaces));
    }
    properties.put(PROPERTY_TITLE, page.getTitle());
    List<Page> list = new ArrayList<>();
    int maxSize = getMaxSize(limit, ConfigurationValueInteger.MAX_EMBEDDED_IN);
    while (result.executeEmbeddedIn(properties, list) &&
           (list.size() < maxSize)) {
      //
    }
    Collections.sort(list);
    page.setRelatedPages(Page.RelatedPages.EMBEDDED_IN, list);
  }
}
