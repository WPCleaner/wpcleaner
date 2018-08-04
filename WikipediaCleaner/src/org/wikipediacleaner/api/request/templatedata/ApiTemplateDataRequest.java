/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.templatedata;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TemplateData;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API TemplateData requests.
 */
public class ApiTemplateDataRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Page identifiers.
   */
  public final static String PROPERTY_PAGEIDS = "pageids";

  /**
   * Property for Redirects.
   */
  public final static String PROPERTY_REDIRECTS = "redirects";

  /**
   * Property for Titles.
   */
  public final static String PROPERTY_TITLES = "titles";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiTemplateDataResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiTemplateDataRequest(EnumWikipedia wiki, ApiTemplateDataResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Delete one page.
   * 
   * @param page Page for which TemplateData is requested.
   * @return TemplateData for the page.
   * @throws APIException Exception thrown by the API.
   */
  public TemplateData retrieveTemplateData(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_TEMPLATE_DATA, result.getFormat());
    properties.put(PROPERTY_TITLES, page.getTitle());
    properties.put(PROPERTY_REDIRECTS, "");
    return result.executeTemplateData(properties);
  }
}
