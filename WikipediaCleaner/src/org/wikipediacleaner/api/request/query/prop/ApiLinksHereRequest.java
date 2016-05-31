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
import java.util.Map.Entry;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;


/**
 * MediaWiki links here requests.
 */
public class ApiLinksHereRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "lhlimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "lhprop";

  /**
   * Property value for Properties / Page identifier.
   */
  public final static String PROPERTY_PROPERTIES_PAGEID = "pageid";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROPERTIES_TITLE = "title";

  /**
   * Property value for Properties / Redirect.
   */
  public final static String PROPERTY_PROPERTIES_REDIRECT = "redirect";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "lhnamespace";

  /**
   * Property for Redirect.
   */
  public final static String PROPERTY_SHOW = "lhshow";

  /**
   * Property for Redirect / Redirect only.
   */
  public final static String PROPERTY_SHOW_REDIRECT = "redirect";

  /**
   * Property for Redirect / No redirect only.
   */
  public final static String PROPERTY_SHOW_NOREDIRECT = "!redirect";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLinksHereResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLinksHereRequest(EnumWikipedia wiki, ApiLinksHereResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of links.
   * 
   * @param page Page for which links to it are requested.
   * @param redirects True if it should also retrieve links through redirects.
   */
  public void loadLinksHere(Page page, boolean redirects) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_PROP,
        PROPERTY_PROP_LINKSHERE);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    List<Page> pages = new ArrayList<>();
    pages.add(page);
    if (redirects) {
      pages.addAll(page.getRelatedPages(RelatedPages.REDIRECTS));
    }
    List<Collection<Page>> splitPagesList = splitListPages(pages, MAX_PAGES_PER_QUERY);
    Map<String, List<Page>> results = new HashMap<>();
    for (Collection<Page> splitPages : splitPagesList) {
      properties.put(PROPERTY_TITLES, constructListTitles(splitPages));
      while (result.executeLinksHere(properties, page, results)) {
        //
      }
    }
    for (Entry<String, List<Page>> tmpResult : results.entrySet()) {
      Collections.sort(tmpResult.getValue());
      for (Page tmpPage : pages) {
        if (Page.areSameTitle(tmpResult.getKey(), tmpPage.getTitle())) {
          tmpPage.setRelatedPages(Page.RelatedPages.LINKS_HERE, tmpResult.getValue());
        }
      }
    }
  }
}
