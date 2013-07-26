/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki back links requests.
 */
public class ApiBacklinksRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Filter redirection.
   */
  public final static String PROPERTY_FILTERREDIR = "blfilterredir";

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
  public final static String PROPERTY_LIMIT = "bllimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "blnamespace";

  /**
   * Property for Redirect.
   */
  public final static String PROPERTY_REDIRECT = "blredirect";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "bltitle";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiBacklinksResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiBacklinksRequest(EnumWikipedia wiki, ApiBacklinksResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of back links.
   * 
   * @param page Page for which back links are requested.
   * @param redirects True if it should also retrieve links through redirects.
   */
  public void loadBacklinks(Page page, boolean redirects) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_BACKLINKS);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    if (redirects) {
      properties.put(PROPERTY_REDIRECT, "");
    }
    properties.put(PROPERTY_TITLE, page.getTitle());
    List<Page> list = new ArrayList<Page>();
    while (result.executeBacklinks(properties, page, list)) {
      //
    }
    Collections.sort(list);
    page.setRelatedPages(Page.RelatedPages.BACKLINKS, list);
  }
}
