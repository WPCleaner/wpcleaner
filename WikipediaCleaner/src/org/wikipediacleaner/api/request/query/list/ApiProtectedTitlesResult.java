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
 * Base interface for MediaWiki API protected titles results.
 */
public interface ApiProtectedTitlesResult extends ApiResult {

  /**
   * Execute protected titles request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with protected titles.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean executeProtectedTitles(
      Map<String, String> properties,
      List<Page> list) throws APIException;
}
