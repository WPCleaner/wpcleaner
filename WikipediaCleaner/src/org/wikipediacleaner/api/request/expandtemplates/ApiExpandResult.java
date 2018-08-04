/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.expandtemplates;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API expand results.
 */
public interface ApiExpandResult extends ApiResult {

  /**
   * Execute expand templates request.
   * 
   * @param properties Properties defining request.
   * @return Expanded text.
   * @throws APIException Exception thrown by the API.
   */
  public String executeExpandTemplates(Map<String, String> properties) throws APIException;
}
