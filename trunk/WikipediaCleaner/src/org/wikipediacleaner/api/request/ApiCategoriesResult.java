/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


/**
 * Base interface for MediaWiki API categories results.
 */
public interface ApiCategoriesResult extends ApiPropertiesResult {

  /**
   * Execute categories request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List to be filled with categories.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeCategories(
      Map<String, String> properties,
      Page page,
      List<Page> list) throws APIException;

  /**
   * Set disambiguation status of a list of pages.
   * 
   * @param properties Properties defining request.
   * @param list List of pages for which disambiguation status needs to be set.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean setDiambiguationStatus(
      Map<String, String> properties,
      Collection<Page> list) throws APIException;
}
