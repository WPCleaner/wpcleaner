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
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki API raw watch list requests.
 */
public class ApiRawWatchlistRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "wrnamespace";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "wrlimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "wrprop";

  /**
   * Property for Properties / Changed.
   */
  public final static String PROPERTY_PROP_CHANGED = "changed";

  /**
   * Property for Show.
   */
  public final static String PROPERTY_SHOW = "wrshow";

  /**
   * Property for Show / Changed.
   */
  public final static String PROPERTY_SHOW_CHANGED = "changed";

  /**
   * Property for Show / Not Changed.
   */
  public final static String PROPERTY_SHOW_NOT_CHANGED = "!changed";

  /**
   * Property for Owner.
   */
  public final static String PROPERTY_OWNER = "wrowner";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "wrtoken";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiRawWatchlistResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiRawWatchlistRequest(EnumWikipedia wiki, ApiRawWatchlistResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load watch list raw.
   * 
   * @return List of pages in the watch list.
   */
  public List<Page> loadWatchlistRaw() throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_WATCHLISTRAW);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    List<Page> watchlist = new ArrayList<Page>();
    while (result.executeWatchlistRaw(properties, watchlist)) {
      //
    }
    return watchlist;
  }
}
