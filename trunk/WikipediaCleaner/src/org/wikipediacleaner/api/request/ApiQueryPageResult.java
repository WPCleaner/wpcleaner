/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


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
   * @throws APIException
   */
  public boolean executeQueryPage(
      Map<String, String> properties,
      List<Page> list) throws APIException;
}
