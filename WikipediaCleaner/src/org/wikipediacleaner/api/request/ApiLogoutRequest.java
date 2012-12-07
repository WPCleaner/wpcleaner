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
 * MediaWiki API logout requests.
 */
public class ApiLogoutRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLogoutResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLogoutRequest(EnumWikipedia wiki, ApiLogoutResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Logout.
   */
  public void logout() throws APIException {
    Map<String, String> properties = getProperties(ACTION_LOGOUT, result.getFormat());
    result.executeLogout(properties);
  }
}
