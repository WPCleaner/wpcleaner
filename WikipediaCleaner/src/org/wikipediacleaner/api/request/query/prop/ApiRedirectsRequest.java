/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki redirects requests.
 */
public class ApiRedirectsRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "rdlimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "rdprop";

  /**
   * Property value for Properties / Page identifier.
   */
  public final static String PROPERTY_PROPERTIES_PAGEID = "pageid";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROPERTIES_TITLE = "title";

  /**
   * Property value for Properties / Fragment.
   */
  public final static String PROPERTY_PROPERTIES_FRAGMENT = "fragment";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "rdnamespace";

  /**
   * Property for Show.
   */
  public final static String PROPERTY_SHOW = "rdshow";

  /**
   * Property for Show / Redirects with a fragment.
   */
  public final static String PROPERTY_SHOW_FRAGMENT = "fragment";

  /**
   * Property for Show / Redirects without a fragment.
   */
  public final static String PROPERTY_SHOW_NOFRAGMENT = "!fragment";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiRedirectsResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiRedirectsRequest(EnumWikipedia wiki, ApiRedirectsResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load redirects.
   * 
   * @param page Page for which redirects to it are requested.
   * @throws APIException Exception thrown by the API.
   */
  public void loadRedirects(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_PROP,
        PROPERTY_PROP_REDIRECTS);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_TITLES, page.getTitle());
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    List<Page> list = new ArrayList<>();
    while (result.executeRedirects(properties, page, list)) {
      //
    }
    Collections.sort(list);
    page.setRelatedPages(Page.RelatedPages.REDIRECTS, list);
  }
}
