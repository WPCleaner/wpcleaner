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
 * Base interface for MediaWiki API back links results.
 */
public interface ApiLinksHereResult extends ApiResult {

  /**
   * Execute links here request.
   * 
   * @param properties Properties defining request.
   * @param page Main page.
   * @param lists Lists to be filled with links to the page.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeLinksHere(
      Map<String, String> properties,
      Page page,
      Map<String, List<Page>> lists) throws APIException;
}
