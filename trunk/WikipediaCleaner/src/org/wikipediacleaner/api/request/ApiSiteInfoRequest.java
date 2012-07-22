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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.APIException;


/**
 * MediaWiki API site information requests.
 */
public class ApiSiteInfoRequest extends ApiMetaRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_SIPROP = "siprop";

  /**
   * Property for Properties / Name spaces.
   */
  public final static String PROPERTY_SIPROP_NAMESPACES = "namespaces";

  /**
   * Property for Properties / Name space aliases.
   */
  public final static String PROPERTY_SIPROP_NAMESPACE_ALIASES = "namespacealiases";

  /**
   * Property for Properties / Languages.
   */
  public final static String PROPERTY_SIPROP_LANGUAGES = "languages";

  /**
   * Property for Properties / Interwiki map.
   */
  public final static String PROPERTY_SIPROP_INTERWIKI_MAP = "interwikimap";

  /**
   * Property for Properties / Magic words.
   */
  public final static String PROPERTY_SIPROP_MAGIC_WORDS = "magicwords";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiSiteInfoResult result;

  /**
   * @param result Parser for result depending on chosen format.
   */
  public ApiSiteInfoRequest(ApiSiteInfoResult result) {
    this.result = result;
  }

  /**
   * Load site information.
   * 
   * @param namespaces True if information about name spaces are requested.
   * @param namespaceAliases True if information about name spaces aliases are requested.
   * @param languages True if information about languages are requested.
   * @param interwikiMap True if information about interwiki map are requested.
   * @param magicWords True if information about magic words are requested.
   */
  public void loadSiteInformation(
      boolean namespaces, boolean namespaceAliases,
      boolean languages, boolean interwikiMap,
      boolean magicWords) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_META,
        PROPERTY_META_SITEINFO);
    Collection<String> information = new ArrayList<String>();
    if (namespaces) {
      information.add(PROPERTY_SIPROP_NAMESPACES);
    }
    if (namespaceAliases) {
      information.add(PROPERTY_SIPROP_NAMESPACE_ALIASES);
    }
    if (languages) {
      information.add(PROPERTY_SIPROP_LANGUAGES);
    }
    if (interwikiMap) {
      information.add(PROPERTY_SIPROP_INTERWIKI_MAP);
    }
    if (magicWords) {
      information.add(PROPERTY_SIPROP_MAGIC_WORDS);
    }
    properties.put(
        ApiSiteInfoRequest.PROPERTY_SIPROP,
        constructList(information));
    result.executeSiteInformation(properties);
  }
}
