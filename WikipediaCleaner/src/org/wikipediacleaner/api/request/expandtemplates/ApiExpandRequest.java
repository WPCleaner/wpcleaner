/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.expandtemplates;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API expand requests.
 */
public class ApiExpandRequest extends ApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Text.
   */
  public final static String PROPERTY_TEXT = "text";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "title";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiExpandResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiExpandRequest(EnumWikipedia wiki, ApiExpandResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Expand templates.
   * 
   * @param title Page title.
   * @param text Page contents.
   * @return Text with expanded templates.
   */
  public String expandTemplates(String title, String text) throws APIException {
    Map<String, String> properties = getProperties(ACTION_EXPAND, result.getFormat());
    properties.put(PROPERTY_TITLE, title);
    properties.put(PROPERTY_TEXT, text);
    return result.executeExpandTemplates(properties);
  }
}
