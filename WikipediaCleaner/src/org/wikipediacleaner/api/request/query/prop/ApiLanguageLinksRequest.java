/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.HashMap;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki API language links requests.
 */
public class ApiLanguageLinksRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Language.
   */
  public final static String PROPERTY_LANG = "lllang";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "lllimit";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLanguageLinksResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLanguageLinksRequest(EnumWikipedia wiki, ApiLanguageLinksResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Get a language link in a page to a specific wiki.
   * 
   * @param page Page.
   * @param toWiki Destination wiki.
   * @throws APIException Exception thrown by the API.
   * @return Language link.
   */
  public String getLanguageLink(Page page, EnumWikipedia toWiki) throws APIException {

    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_PROP, PROPERTY_PROP_LANGLINKS);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    String toWikiCode = toWiki.getSettings().getCode();
    properties.put(PROPERTY_LANG, toWikiCode);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_REDIRECTS, "");
    properties.put(PROPERTY_TITLES, page.getTitle());
    Map<String, String> languageLinks = new HashMap<String, String>();
    while (result.getLanguageLinks(properties, languageLinks) &&
           !languageLinks.containsKey(toWikiCode)) {
      //
    }
    return languageLinks.get(toWikiCode);
  }
}
