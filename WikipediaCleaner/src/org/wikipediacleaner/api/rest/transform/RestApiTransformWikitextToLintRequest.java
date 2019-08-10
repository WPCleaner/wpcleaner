/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.rest.transform;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.api.rest.RestApiRequest;


/**
 * MediaWiki REST API "Transform Wikitext To Lint" requests.
 */
public class RestApiTransformWikitextToLintRequest extends RestApiRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /** Property for WikiText */
  public final static String PROPERTY_WIKITEXT = "wikitext";

  // ==========================================================================
  // Request management
  // ==========================================================================

  /** Result */
  private final RestApiTransformWikitextToLintResult result;

  /**
   * Constructor.
   * 
   * @param wiki Wiki.
   * @param result Parser for the result.
   */
  public RestApiTransformWikitextToLintRequest(EnumWikipedia wiki, RestApiTransformWikitextToLintResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * @param title Page title.
   * @param text Wiki text.
   * @return List of linter errors for the text.
   * @throws APIException Exception thrown by the API.
   */
  public List<LinterError> transform(String title, String text) throws APIException {
    Map<String, String> properties = getProperties();
    if (text != null) {
      properties.put(PROPERTY_WIKITEXT, text);
    }
    return result.transform(title, properties);
  }
}
