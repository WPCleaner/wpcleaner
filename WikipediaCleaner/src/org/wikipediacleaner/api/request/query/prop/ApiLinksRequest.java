/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki links requests.
 */
public class ApiLinksRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "pllimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "plnamespace";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLinksResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLinksRequest(EnumWikipedia wiki, ApiLinksResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of links.
   * 
   * @param pages List of pages for which links are requested.
   * @throws APIException Exception thrown by the API.
   */
  public void loadLinks(Collection<Page> pages) throws APIException {
    List<Collection<Page>> splitPagesList = splitListPages(pages, MAX_PAGES_PER_QUERY);
    for (Collection<Page> splitPages : splitPagesList) {
      Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
      properties.put(PROPERTY_PROP, PROPERTY_PROP_LINKS);
      properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
      properties.put(PROPERTY_LIMIT, LIMIT_MAX);
      properties.put(PROPERTY_TITLES, constructListTitles(splitPages));
      Map<String, List<Page>> lists = new HashMap<>();
      Map<String, String> normalization = new HashMap<>();
      while (result.executeLinks(properties, lists, normalization)) {
        //
      }
      for (Page page : splitPages) {
        String pageTitle = page.getTitle();
        if (normalization.containsKey(pageTitle)) {
          pageTitle = normalization.get(pageTitle);
        }
        List<Page> list = lists.get(pageTitle);
        if (list != null) {
          Collections.sort(list);
        }
        page.setLinks(list);
      }
    }
  }

  /**
   * Load list of links.
   * 
   * @param page Page for which links are requested.
   * @param namespace Restrict the list to a given namespace.
   * @param knownPages Already known pages.
   * @param redirects List of redirects filled by the method.
   * @param disambig True if disambiguation information is requested.
   * @throws APIException Exception thrown by the API.
   */
  public void loadLinks(
      Page page, Integer namespace,
      List<Page> knownPages,
      List<Page> redirects, boolean disambig) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_GENERATOR, PROPERTY_PROP_LINKS);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    if (disambig) {
      properties.put(
          PROPERTY_PROP,
          PROPERTY_PROP_PAGEPROPS + "|" + PROPERTY_PROP_INFO);
      properties.put(
          ApiPagePropsRequest.PROPERTY_PROPERTIES,
          ApiPagePropsRequest.PROPERTY_PROPERTIES_DISAMBIGUATION);
    } else {
      properties.put(
          PROPERTY_PROP,
          PROPERTY_PROP_INFO);
    }
    if (namespace != null) {
      properties.put(GENERATOR_PREFIX + PROPERTY_NAMESPACE, namespace.toString());
    }
    properties.put(PROPERTY_TITLES, page.getTitle());
    properties.put(GENERATOR_PREFIX + PROPERTY_LIMIT, LIMIT_MAX);
    List<Page> links = new ArrayList<>();
    while (result.executeLinks(properties, links, knownPages, null, redirects, disambig)) {
      //
    }
    page.setLinks(links);
  }
}
