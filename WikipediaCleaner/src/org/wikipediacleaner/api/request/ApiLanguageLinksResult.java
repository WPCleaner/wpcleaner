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


/**
 * Base interface for MediaWiki API language links results.
 */
public interface ApiLanguageLinksResult extends ApiResult {

  /**
   * Get language links of a page.
   * 
   * @param properties Properties defining request.
   * @param languageLinks Map of language links to be set.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean getLanguageLinks(
      Map<String, String> properties,
      Map<String, String> languageLinks) throws APIException;
}
