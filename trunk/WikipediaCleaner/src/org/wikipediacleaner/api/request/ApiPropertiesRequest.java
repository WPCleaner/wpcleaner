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


/**
 * Base class MediaWiki API properties query requests.
 */
public class ApiPropertiesRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Page identifiers.
   */
  public final static String PROPERTY_PAGEIDS = "pageids";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "prop";

  /**
   * Property value for Properties / Templates.
   */
  public final static String PROPERTY_PROP_TEMPLATES = "templates";

  /**
   * Property for Redirects.
   */
  public final static String PROPERTY_REDIRECTS = "redirects";

  /**
   * Property for Revision identifiers.
   */
  public final static String PROPERTY_REVIDS = "revids";

  /**
   * Property for Titles.
   */
  public final static String PROPERTY_TITLES = "titles";

  // ==========================================================================
  // Configuration
  // ==========================================================================

  public final static int MAX_PAGES_PER_QUERY = 50;
}
