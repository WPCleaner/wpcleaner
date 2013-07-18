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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AbuseFilter;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki abuse log requests.
 */
public class ApiAbuseLogRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "afldir";

  /**
   * Property value for Direction / Newer.
   */
  public final static String PROPERTY_DIR_NEWER = "newer";

  /**
   * Property value for Direction / Older.
   */
  public final static String PROPERTY_DIR_OLDER = "older";

  /**
   * Property for End.
   */
  public final static String PROPERTY_END = "aflend";

  /**
   * Property for Filter.
   */
  public final static String PROPERTY_FILTER = "aflfilter";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "afllimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "aflprop";

  /**
   * Property value for Properties / Action.
   */
  public final static String PROPERTY_PROP_ACTION = "action";

  /**
   * Property value for Properties / Details.
   */
  public final static String PROPERTY_PROP_DETAILS = "details";

  /**
   * Property value for Properties / Filter.
   */
  public final static String PROPERTY_PROP_FILTER = "filter";

  /**
   * Property value for Properties / Hidden.
   */
  public final static String PROPERTY_PROP_HIDDEN = "hidden";

  /**
   * Property value for Properties / Identifiers.
   */
  public final static String PROPERTY_PROP_IDS = "ids";

  /**
   * Property value for Properties / IP.
   */
  public final static String PROPERTY_PROP_IP = "ip";

  /**
   * Property value for Properties / Result.
   */
  public final static String PROPERTY_PROP_RESULT = "result";

  /**
   * Property value for Properties / Timestamp.
   */
  public final static String PROPERTY_PROP_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROP_TITLE = "title";

  /**
   * Property value for Properties / User.
   */
  public final static String PROPERTY_PROP_USER = "user";

  /**
   * Property for Start.
   */
  public final static String PROPERTY_START = "aflstart";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "afltitle";

  /**
   * Property for User.
   */
  public final static String PROPERTY_USER = "afluser";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiAbuseLogResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiAbuseLogRequest(EnumWikipedia wiki, ApiAbuseLogResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of pages that triggered an abuse filters.
   */
  public List<Page> loadAbuseLog(AbuseFilter filter) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_ABUSELOG);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    if (filter != null) {
      properties.put(PROPERTY_FILTER, Integer.toString(filter.getId()));
    }
    List<Page> list = new ArrayList<Page>();
    while (result.executeAbuseLog(properties, list)) {
      //
    }
    return list;
  }
}
