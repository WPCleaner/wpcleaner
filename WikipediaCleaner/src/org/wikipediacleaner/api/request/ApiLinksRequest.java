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
   */
  public void loadLinks(Collection<Page> pages) throws APIException {
    List<Collection<Page>> splitPagesList = splitListPages(pages, MAX_PAGES_PER_QUERY);
    for (Collection<Page> splitPages : splitPagesList) {
      Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
      properties.put(
          PROPERTY_PROP,
          PROPERTY_PROP_LINKS);
      properties.put(PROPERTY_LIMIT, LIMIT_MAX);
      properties.put(PROPERTY_TITLES, constructListPages(splitPages));
      Map<String, List<Page>> lists = new HashMap<String, List<Page>>();
      while (result.executeLinks(properties, lists)) {
        //
      }
      for (Page page : splitPages) {
        List<Page> list = lists.get(page.getTitle());
        if (list != null) {
          Collections.sort(list);
        }
        page.setLinks(list);
      }
    }
  }
}
