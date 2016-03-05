/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.tokens;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API tokens results.
 */
public interface ApiTokensResult extends ApiResult {

  /**
   * Execute tokens request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public void executeTokens(Map<String, String> properties) throws APIException;
}
