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
 * MediaWiki API parse requests.
 */
public class ApiParseRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Text.
   */
  public final static String PROPERTY_TEXT = "text";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "title";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiParseResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiParseRequest(EnumWikipedia wiki, ApiParseResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Parse.
   * 
   * @param title Page title.
   * @param text Page contents.
   * @return Parsed text.
   */
  public String parseText(String title, String text) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PARSE, result.getFormat());
    properties.put(PROPERTY_TITLE, title);
    properties.put(PROPERTY_TEXT, text);
    return result.executeParse(properties);
  }
}
