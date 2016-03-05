/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API users results.
 */
public interface ApiUsersResult extends ApiResult {

  /**
   * Execute user request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public User executeUser(Map<String, String> properties) throws APIException;
}
