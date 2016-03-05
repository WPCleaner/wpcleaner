/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.parse;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Section;
import org.wikipediacleaner.api.request.ApiResult;


/**
 * Base interface for MediaWiki API parse results.
 */
public interface ApiParseResult extends ApiResult {

  /**
   * Execute parse request.
   * 
   * @param properties Properties defining request.
   * @return Expanded text.
   * @throws APIException
   */
  public String executeParse(Map<String, String> properties) throws APIException;

  /**
   * Execute sections request.
   * 
   * @param page Page.
   * @param properties Properties defining request.
   * @return List of sections.
   * @throws APIException
   */
  public List<Section> executeSections(Page page, Map<String, String> properties) throws APIException;
}
