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
 * Base interface for MediaWiki API raw watch list results.
 */
public interface ApiRawWatchlistResult extends ApiResult {

  /**
   * Execute raw watch list request.
   * 
   * @param properties Properties defining request.
   * @param watchlist List of pages to be filled with the watch list.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean executeWatchlistRaw(
      Map<String, String> properties,
      List<Page> watchlist) throws APIException;
}
