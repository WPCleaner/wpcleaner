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
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API properties results.
 */
public interface ApiPropertiesResult extends ApiResult {

  /**
   * Execute redirect request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with redirect information.
   * @throws APIException Exception thrown by the API.
   */
  public void executeRedirect(
      Map<String, String> properties,
      List<Page> pages) throws APIException;
}
