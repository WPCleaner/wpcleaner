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

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


/**
 * Base interface for MediaWiki API category members results.
 */
public interface ApiCategoryMembersResult extends ApiResult {

  /**
   * Execute category members request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with category members.
   * @param categories Map of categories to be analyzed with their depth.
   * @param depth Current depth of the analysis.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeCategoryMembers(
      Map<String, String> properties,
      List<Page> list,
      Map<Page, Integer> categories, int depth) throws APIException;
}
