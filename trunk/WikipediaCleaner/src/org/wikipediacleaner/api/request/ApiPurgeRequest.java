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

import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki API purge requests.
 */
public class ApiPurgeRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Titles.
   */
  public final static String PROPERTY_TITLES = "titles";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiPurgeResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiPurgeRequest(EnumWikipedia wiki, ApiPurgeResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Purge one page.
   * 
   * @param page Page to be purged.
   */
  public void purgePage(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PURGE, result.getFormat());
    properties.put(PROPERTY_TITLES, page.getTitle());
    result.executePurge(properties);
  }

  /**
   * Purge pages.
   * 
   * @param pages Pages to be purged.
   */
  public void purgePages(Collection<Page> pages) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PURGE, result.getFormat());
    properties.put(PROPERTY_TITLES, constructListTitles(pages));
    result.executePurge(properties);
  }
}
