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
 * Base interface for MediaWiki API category members results.
 */
public interface ApiLintErrorsResult extends ApiResult {

  /**
   * Execute lint errors request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with lint errors.
   * @param category Linter category.
   * @param withTemplates Includes templates causing the error.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeLinterCategory(
      Map<String, String> properties,
      List<Page> list,
      String category, boolean withTemplates) throws APIException;
}
