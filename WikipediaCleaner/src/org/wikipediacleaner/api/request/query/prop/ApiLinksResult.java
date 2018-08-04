/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


/**
 * Base interface for MediaWiki API links results.
 */
public interface ApiLinksResult extends ApiPropertiesResult {

  /**
   * Execute links request.
   * 
   * @param properties Properties defining request.
   * @param lists Map of lists to be filled with links.
   * @param normalization Map containing information about title normalization (key=From, value=To).
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean executeLinks(
      Map<String, String> properties,
      Map<String, List<Page>> lists,
      Map<String, String> normalization) throws APIException;

  /**
   * Execute links request.
   * 
   * @param properties Properties defining request.
   * @param links List to be filled with links.
   * @param knownPages Already known pages.
   * @param normalization Map containing information about title normalization (key=From, value=To).
   * @param redirects List of redirects filled by the method.
   * @param useDisambig Flag indicating if disambiguation property should be used.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean executeLinks(
      Map<String, String> properties,
      List<Page> links,
      List<Page> knownPages,
      Map<String, String> normalization,
      List<Page> redirects, boolean useDisambig) throws APIException;
}
