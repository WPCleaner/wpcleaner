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

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki API delete requests.
 */
public class ApiDeleteRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Old image.
   */
  public final static String PROPERTY_OLDIMAGE = "oldimage";

  /**
   * Property for Page identifier.
   */
  public final static String PROPERTY_PAGEID = "pageid";

  /**
   * Property for Reason.
   */
  public final static String PROPERTY_REASON = "reason";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "title";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "token";

  /**
   * Property for Unwatch.
   */
  public final static String PROPERTY_UNWATCH = "unwatch";

  /**
   * Property for Watch.
   */
  public final static String PROPERTY_WATCH = "watch";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiDeleteResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiDeleteRequest(EnumWikipedia wiki, ApiDeleteResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Delete one page.
   * 
   * @param page Page to be deleted.
   * @param reason Reason for deletion.
   */
  public void deletePage(Page page, String reason) throws APIException {
    Map<String, String> properties = getProperties(ACTION_DELETE, result.getFormat());
    if (reason != null) {
      properties.put(PROPERTY_REASON, reason);
    }
    properties.put(PROPERTY_TITLE, page.getTitle());
    properties.put(PROPERTY_TOKEN, getWiki().getConnection().getDeleteToken());
    result.executeDelete(properties);
  }
}
