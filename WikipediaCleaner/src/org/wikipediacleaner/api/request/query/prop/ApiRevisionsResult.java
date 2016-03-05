/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.data.Page;


/**
 * Base interface for MediaWiki API revisions results.
 */
public interface ApiRevisionsResult extends ApiPropertiesResult {

  /**
   * Execute last revision request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with last revision content.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeLastRevision(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException;
}
