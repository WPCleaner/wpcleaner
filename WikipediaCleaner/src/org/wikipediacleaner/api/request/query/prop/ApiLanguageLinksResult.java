/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.Map;

import org.wikipediacleaner.api.APIException;


/**
 * Base interface for MediaWiki API language links results.
 */
public interface ApiLanguageLinksResult extends ApiPropertiesResult {

  /**
   * Get language links of a page.
   * 
   * @param properties Properties defining request.
   * @param languageLinks Map of language links to be set.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean getLanguageLinks(
      Map<String, String> properties,
      Map<String, String> languageLinks) throws APIException;
}
