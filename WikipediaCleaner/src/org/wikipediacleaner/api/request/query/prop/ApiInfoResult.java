/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


/**
 * Base interface for MediaWiki API informations results.
 */
public interface ApiInfoResult extends ApiPropertiesResult {

  /**
   * Execute informations request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with informations.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeInformations(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException;
}
