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
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


/**
 * Base interface for MediaWiki API templates results.
 */
public interface ApiTemplatesResult extends ApiPropertiesResult {

  /**
   * Execute templates request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List to be filled with templates.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeTemplates(
      Map<String, String> properties,
      Page page,
      List<Page> list) throws APIException;

  /**
   * Set disambiguation status of a list of pages.
   * 
   * @param properties Properties defining request.
   * @param list List of pages for which disambiguation status needs to be set.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean setDiambiguationStatus(
      Map<String, String> properties,
      Collection<Page> list) throws APIException;
}
