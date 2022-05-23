/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API query page results.
 */
public interface ApiQueryPageResult extends ApiResult {

  /**
   * Execute query page request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with query pages.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean executeQueryPage(
      Map<String, String> properties,
      Set<Page> list) throws APIException;
}
