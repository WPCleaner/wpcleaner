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


/**
 * MediaWiki API tokens requests.
 */
public class ApiTokensRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Type.
   */
  public final static String PROPERTY_TYPE = "type";

  /**
   * Property value for type / Block.
   */
  public final static String PROPERTY_TYPE_BLOCK = "block";

  /**
   * Property value for type / Delete.
   */
  public final static String PROPERTY_TYPE_DELETE = "delete";

  /**
   * Property value for type / Edit.
   */
  public final static String PROPERTY_TYPE_EDIT = "edit";

  /**
   * Property value for type / Email.
   */
  public final static String PROPERTY_TYPE_EMAIL = "email";

  /**
   * Property value for type / Import.
   */
  public final static String PROPERTY_TYPE_IMPORT = "import";

  /**
   * Property value for type / Move.
   */
  public final static String PROPERTY_TYPE_MOVE = "move";

  /**
   * Property value for type / Options.
   */
  public final static String PROPERTY_TYPE_OPTIONS = "options";

  /**
   * Property value for type / Patrol.
   */
  public final static String PROPERTY_TYPE_PATROL = "patrol";

  /**
   * Property value for type / Protect.
   */
  public final static String PROPERTY_TYPE_PROTECT = "protect";

  /**
   * Property value for type / Unblock.
   */
  public final static String PROPERTY_TYPE_UNBLOCK = "unblock";

  /**
   * Property value for type / Watch.
   */
  public final static String PROPERTY_TYPE_WATCH = "watch";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiTokensResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiTokensRequest(EnumWikipedia wiki, ApiTokensResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Retrieve tokens.
   */
  public void retrieveTokens() throws APIException {
    Map<String, String> properties = getProperties(ACTION_TOKENS, result.getFormat());
    properties.put(PROPERTY_TYPE, PROPERTY_TYPE_EDIT);
    result.executeTokens(properties);
  }
}
