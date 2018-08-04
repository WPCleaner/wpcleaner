/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.templatedata;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.TemplateData;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API TemplateData results.
 */
public interface ApiTemplateDataResult extends ApiResult {

  /**
   * Execute TemplateData request.
   * 
   * @param properties Properties defining request.
   * @return TemplateData for the page.
   * @throws APIException Exception thrown by the API.
   */
  public TemplateData executeTemplateData(Map<String, String> properties) throws APIException;
}
