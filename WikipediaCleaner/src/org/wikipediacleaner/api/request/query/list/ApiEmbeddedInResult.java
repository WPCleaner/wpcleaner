/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API embedded in results.
 */
public interface ApiEmbeddedInResult extends ApiResult {

  /**
   * Execute embedded in request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with embedding pages.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeEmbeddedIn(
      Map<String, String> properties,
      List<Page> list) throws APIException;
}
