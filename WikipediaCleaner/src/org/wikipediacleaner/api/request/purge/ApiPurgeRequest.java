/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.purge;

import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API purge requests.
 */
public class ApiPurgeRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Forcing links update.
   */
  public final static String PROPERTY_FORCE_LINK_UPDATE = "forcelinkupdate";

  /**
   * Property for Titles.
   */
  public final static String PROPERTY_TITLES = "titles";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiPurgeResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiPurgeRequest(EnumWikipedia wiki, ApiPurgeResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Purge one page.
   * 
   * @param page Page to be purged.
   * @throws APIException Exception thrown by the API.
   */
  public void purgePage(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PURGE, result.getFormat());
    properties.put(PROPERTY_TITLES, page.getTitle());
    properties.put(PROPERTY_FORCE_LINK_UPDATE, "");
    result.executePurge(properties);
  }

  /**
   * Purge pages.
   * 
   * @param pages Pages to be purged.
   * @throws APIException Exception thrown by the API.
   */
  public void purgePages(Collection<Page> pages) throws APIException {
    Map<String, String> properties = getProperties(ACTION_PURGE, result.getFormat());
    properties.put(PROPERTY_TITLES, constructListTitles(pages));
    properties.put(PROPERTY_FORCE_LINK_UPDATE, "");
    result.executePurge(properties);
  }
}
