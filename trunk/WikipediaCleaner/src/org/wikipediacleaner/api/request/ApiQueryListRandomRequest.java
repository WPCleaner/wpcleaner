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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;


/**
 * Interface for MediaWiki random list requests.
 */
public class ApiQueryListRandomRequest extends ApiQueryMetaRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "rnlimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "rnnamespace";

  /**
   * Property for Redirect.
   */
  public final static String PROPERTY_REDIRECT = "rnredirect";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiQueryListRandomResult result;

  /**
   * @param result Parser for result depending on chosen format.
   */
  public ApiQueryListRandomRequest(ApiQueryListRandomResult result) {
    this.result = result;
  }

  /**
   * Load list of random pages.
   * 
   * @param count Maximum number of pages to get.
   * @return List of random pages.
   */
  public List<Page> loadRandomList(int count) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        ApiQueryListRequest.PROPERTY_LIST,
        ApiQueryListRequest.PROPERTY_LIST_RANDOM);
    properties.put(PROPERTY_LIMIT, Integer.toString(count));
    properties.put(PROPERTY_NAMESPACE, Integer.toString(Namespace.MAIN));
    List<Page> list = new ArrayList<Page>();
    result.executeRandomList(properties, list);
    return list;
  }
}
