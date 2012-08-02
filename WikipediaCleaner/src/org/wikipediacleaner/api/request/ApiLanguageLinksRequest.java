/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.request;

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
   */
  public String getLanguageLink(Page page, EnumWikipedia toWiki) throws APIException {

    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_PROP,
        PROPERTY_PROP_LANGLINKS);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_TITLES, page.getTitle());
    Map<String, String> languageLinks = new HashMap<String, String>();
    String toWikiCode = toWiki.getSettings().getCode();
    while (result.getLanguageLinks(properties, languageLinks) &&
           !languageLinks.containsKey(toWikiCode)) {
      //
    }
    return languageLinks.get(toWikiCode);
  }
}
