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
import org.wikipediacleaner.api.data.RecentChange;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API recent changes results.
 */
public interface ApiRecentChangesResult extends ApiResult {

  /**
   * Execute recent changes request.
   * 
   * @param properties Properties defining request.
   * @param recentChanges The list of recent changes to be filled.
   * @return The timestamp to use as a starting point for the next call.
   * @throws APIException Exception thrown by the API.
   */
  public String executeRecentChanges(
      Map<String, String> properties,
      List<RecentChange> recentChanges) throws APIException;
}
