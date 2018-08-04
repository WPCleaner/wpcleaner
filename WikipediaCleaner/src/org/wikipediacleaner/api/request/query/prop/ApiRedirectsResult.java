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
 * Base interface for MediaWiki API redirects results.
 */
public interface ApiRedirectsResult extends ApiResult {

  /**
   * Execute redirects request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List to be filled with links to the page.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  public boolean executeRedirects(
      Map<String, String> properties,
      Page page,
      List<Page> list) throws APIException;
}
