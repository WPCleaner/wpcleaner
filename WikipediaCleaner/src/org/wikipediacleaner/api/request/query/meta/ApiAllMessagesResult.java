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

  /**
   * Execute messages request.
   * 
   * @param properties Properties defining request.
   * @param messages Map of messages to be filled with the results.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeMessages(
      Map<String, String> properties,
      Map<String, String> messages) throws APIException;
}
