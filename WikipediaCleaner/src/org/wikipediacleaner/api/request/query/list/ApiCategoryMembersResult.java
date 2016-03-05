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
public interface ApiCategoryMembersResult extends ApiResult {

  /**
   * Execute category members request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with category members.
   * @param categories Map of categories to be analyzed with their depth.
   * @param depth Current depth of the analysis.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeCategoryMembers(
      Map<String, String> properties,
      List<Page> list,
      Map<Page, Integer> categories, int depth) throws APIException;
}
