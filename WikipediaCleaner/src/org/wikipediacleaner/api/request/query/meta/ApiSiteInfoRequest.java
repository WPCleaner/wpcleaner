/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;


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
  public final static String PROPERTY_PROP = "siprop";

  /**
   * Property for Properties / General.
   */
  public final static String PROPERTY_PROP_GENERAL = "general";

  /**
   * Property for Properties / Interwiki map.
   */
  public final static String PROPERTY_PROP_INTERWIKI_MAP = "interwikimap";

  /**
   * Property for Properties / Languages.
   */
  public final static String PROPERTY_PROP_LANGUAGES = "languages";

  /**
   * Property for Properties / Magic words.
   */
  public final static String PROPERTY_PROP_MAGIC_WORDS = "magicwords";

  /**
   * Property for Properties / Name space aliases.
   */
  public final static String PROPERTY_PROP_NAMESPACE_ALIASES = "namespacealiases";

  /**
   * Property for Properties / Name spaces.
   */
  public final static String PROPERTY_PROP_NAMESPACES = "namespaces";

  /**
   * Property for Properties / Statistics.
   */
  public final static String PROPERTY_PROP_STATISTICS = "statistics";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiSiteInfoResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiSiteInfoRequest(EnumWikipedia wiki, ApiSiteInfoResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load site information.
   * 
   * @param general True if general information are requested.
   * @param namespaces True if information about name spaces are requested.
   * @param namespaceAliases True if information about name spaces aliases are requested.
   * @param languages True if information about languages are requested.
   * @param interwikiMap True if information about interwiki map are requested.
   * @param magicWords True if information about magic words are requested.
   */
  public void loadSiteInformation(
      boolean general,
      boolean namespaces, boolean namespaceAliases,
      boolean languages, boolean interwikiMap,
      boolean magicWords) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_META,
        PROPERTY_META_SITEINFO);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    Collection<String> information = new ArrayList<String>();
    if (general) {
      information.add(PROPERTY_PROP_GENERAL);
    }
    if (namespaces) {
      information.add(PROPERTY_PROP_NAMESPACES);
    }
    if (namespaceAliases) {
      information.add(PROPERTY_PROP_NAMESPACE_ALIASES);
    }
    if (languages) {
      information.add(PROPERTY_PROP_LANGUAGES);
    }
    if (interwikiMap) {
      information.add(PROPERTY_PROP_INTERWIKI_MAP);
    }
    if (magicWords) {
      information.add(PROPERTY_PROP_MAGIC_WORDS);
    }
    properties.put(
        PROPERTY_PROP,
        constructList(information));
    result.executeSiteInformation(properties);
  }
}
