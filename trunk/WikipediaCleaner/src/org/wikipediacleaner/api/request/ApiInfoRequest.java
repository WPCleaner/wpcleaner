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

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * MediaWiki information requests.
 */
public class ApiInfoRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "inprop";

  /**
   * Property value for Properties / Pre load.
   */
  public final static String PROPERTY_PROPERTIES_PRELOAD = "preload";

  /**
   * Property value for Properties / Protection.
   */
  public final static String PROPERTY_PROPERTIES_PROTECTION = "protection";

  /**
   * Property value for Properties / Subject Id.
   */
  public final static String PROPERTY_PROPERTIES_SUBJECTID = "subjectid";

  /**
   * Property value for Properties / Talk id.
   */
  public final static String PROPERTY_PROPERTIES_TALKID = "talkid";

  /**
   * Property value for Properties / URL.
   */
  public final static String PROPERTY_PROPERTIES_URL = "url";

  /**
   * Property value for Properties / Watched.
   */
  public final static String PROPERTY_PROPERTIES_WATCHED = "watched";

  /**
   * Property for Token.
   */
  public final static String PROPERTY_TOKEN = "intoken";

  /**
   * Property value for Token / Delete.
   */
  public final static String PROPERTY_TOKEN_DELETE = "delete";

  /**
   * Property value for Token / Edit.
   */
  public final static String PROPERTY_TOKEN_EDIT = "edit";

  /**
   * Property value for Token / Move.
   */
  public final static String PROPERTY_TOKEN_MOVE = "move";

  // ==========================================================================
  // Request management
  // ==========================================================================

  /**
   * @param wiki Wiki.
   */
  public ApiInfoRequest(EnumWikipedia wiki) {
    super(wiki);
  }
}
