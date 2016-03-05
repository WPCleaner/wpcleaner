/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API messages results.
 */
public interface ApiAllMessagesResult extends ApiResult {

  /**
   * Execute message request.
   * 
   * @param properties Properties defining request.
   * @return Message.
   * @throws APIException
   */
  public String executeMessage(Map<String, String> properties) throws APIException;
}
