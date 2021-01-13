/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * MediaWiki category members requests.
 */
public class ApiLintErrorsRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /** Property for Categories. */
  public final static String PROPERTY_CATEGORIES = "lntcategories";

  /** Property for From. */
  public final static String PROPERTY_PAGEID = "cmpageid";

  /** Property for Limit. */
  public final static String PROPERTY_LIMIT = "lntlimit";

  /** Property for Name space. */
  public final static String PROPERTY_NAMESPACE = "lntnamespace";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiLintErrorsResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiLintErrorsRequest(EnumWikipedia wiki, ApiLintErrorsResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of lint errors.
   * 
   * @param category Linter category.
   * @param namespace Optional name space.
   * @param withTemplates True to retrieve also templates causing the error.
   * @param limit Flag indicating if the number of results should be limited.
   * @param max Absolute maximum number of results
   * @return List of pages with the error.
   * @throws APIException Exception thrown by the API.
   */
  public List<Page> loadLintErrors(
      String category, Integer namespace, boolean withTemplates,
      boolean limit, int max) throws APIException {

    int maxSize = getMaxSize(limit, ConfigurationValueInteger.MAX_LINT_ERRORS);
    maxSize = Math.min(maxSize, max);
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_LIST, PROPERTY_LIST_LINTERRORS);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_CATEGORIES, category);
    if (namespace != null) {
      properties.put(PROPERTY_NAMESPACE, namespace.toString());
    }
    List<Page> list = new ArrayList<>();
    while (result.executeLinterCategory(
        properties, list, category, withTemplates) &&
        (list.size() < maxSize)) {
      //
    }
    Collections.sort(list);
    return list;
  }
}
