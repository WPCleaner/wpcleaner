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
 * Base interface for MediaWiki API pages with property results.
 */
public interface ApiPagesWithPropResult extends ApiResult {

  /**
   * Execute pages with property request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with protected titles.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executePagesWithProp(
      Map<String, String> properties,
      List<Page> list) throws APIException;
}
