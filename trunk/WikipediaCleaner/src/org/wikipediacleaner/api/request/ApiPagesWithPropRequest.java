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
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * MediaWiki pages with properties requests.
 */
public class ApiPagesWithPropRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Direction.
   */
  public final static String PROPERTY_DIR = "pwpdir";

  /**
   * Property value for Direction / Ascending.
   */
  public final static String PROPERTY_DIR_ASC = "ascending";

  /**
   * Property value for Direction / Descending.
   */
  public final static String PROPERTY_DIR_DESC = "descending";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "pwplimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "pwpprop";

  /**
   * Property value for Properties / Identifiers.
   */
  public final static String PROPERTY_PROPERTIES_IDS = "ids";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROPERTIES_TITLE = "title";

  /**
   * Property value for Properties / Value.
   */
  public final static String PROPERTY_PROPERTIES_VALUE = "value";

  /**
   * Property for Property Name.
   */
  public final static String PROPERTY_PROPERTY_NAME = "pwppropname";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiPagesWithPropResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiPagesWithPropRequest(EnumWikipedia wiki, ApiPagesWithPropResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of pages with a given property.
   * 
   * @param property Property name.
   * @param limit Flag indicating if the number of results should be limited.
   * @return List of protected titles.
   */
  public List<Page> loadPagesWithProp(
      String property,
      boolean limit) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_PAGESWITHPROP);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_PROPERTY_NAME, property);
    List<Page> list = new ArrayList<Page>();
    int maxSize = getMaxSize(limit, ConfigurationValueInteger.MAX_PAGES_WITH_PROP);
    while (result.executePagesWithProp(properties, list) &&
           (list.size() < maxSize)) {
      //
    }
    Collections.sort(list);
    return list;
  }
}
