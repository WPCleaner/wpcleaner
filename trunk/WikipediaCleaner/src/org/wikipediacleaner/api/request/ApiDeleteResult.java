/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.util.Map;

import org.wikipediacleaner.api.APIException;


/**
 * Base interface for MediaWiki API delete results.
 */
public interface ApiDeleteResult extends ApiResult {

  /**
   * Execute delete request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public void executeDelete(Map<String, String> properties) throws APIException;
}
