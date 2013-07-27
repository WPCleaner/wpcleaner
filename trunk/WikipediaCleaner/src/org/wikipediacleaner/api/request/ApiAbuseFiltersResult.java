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
import org.wikipediacleaner.api.data.AbuseFilter;


/**
 * Base interface for MediaWiki API abuse filters results.
 */
public interface ApiAbuseFiltersResult extends ApiResult {

  /**
   * Execute abuse filters request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with abuse filters.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeAbuseFilters(
      Map<String, String> properties,
      List<AbuseFilter> list) throws APIException;
}
