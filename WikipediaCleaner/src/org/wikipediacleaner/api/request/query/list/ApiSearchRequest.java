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
 * MediaWiki search requests.
 */
public class ApiSearchRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Info.
   */
  public final static String PROPERTY_INFO = "srinfo";

  /**
   * Property value for Info / Total hits.
   */
  public final static String PROPERTY_INFO_TOTALHITS = "totalhits";

  /**
   * Property value for Info / Suggestion.
   */
  public final static String PROPERTY_INFO_SUGGESTION = "suggestion";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "srlimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "srnamespace";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "srprop";

  /**
   * Property value for Properties / Has related.
   */
  public final static String PROPERTY_PROP_HASRELATED = "hasrelated";

  /**
   * Property value for Properties / Redirect snippet.
   */
  public final static String PROPERTY_PROP_REDIRECTSNIPPET = "redirectsnippet";

  /**
   * Property value for Properties / Redirect title.
   */
  public final static String PROPERTY_PROP_REDIRECTTITLE = "redirecttitle";

  /**
   * Property value for Properties / Score.
   */
  public final static String PROPERTY_PROP_SCORE = "score";

  /**
   * Property value for Properties / Section snippet.
   */
  public final static String PROPERTY_PROP_SECTIONSNIPPET = "sectionsnippet";

  /**
   * Property value for Properties / Section title.
   */
  public final static String PROPERTY_PROP_SECTIONTITLE = "sectiontitle";

  /**
   * Property value for Properties / Size.
   */
  public final static String PROPERTY_PROP_SIZE = "size";

  /**
   * Property value for Properties / Snippet.
   */
  public final static String PROPERTY_PROP_SNIPPET = "snippet";

  /**
   * Property value for Properties / Time stamp.
   */
  public final static String PROPERTY_PROP_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / Title snippet.
   */
  public final static String PROPERTY_PROP_TITLESNIPPET = "titlesnippet";

  /**
   * Property value for Properties / Word count.
   */
  public final static String PROPERTY_PROP_WORDCOUNT = "wordcount";

  /**
   * Property for Redirects.
   */
  public final static String PROPERTY_REDIRECTS = "srredirects";

  /**
   * Property for Search.
   */
  public final static String PROPERTY_SEARCH = "srsearch";

  /**
   * Property for What.
   */
  public final static String PROPERTY_WHAT = "srwhat";

  /**
   * Property value for What / text.
   */
  public final static String PROPERTY_WHAT_TEXT = "text";

  /**
   * Property value for What / title.
   */
  public final static String PROPERTY_WHAT_TITLE = "title";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiSearchResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiSearchRequest(EnumWikipedia wiki, ApiSearchResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Execute a search.
   * 
   * @param page Page for which similar pages are searched.
   * @param limit Flag indicating if the number of results should be limited.
   */
  public void searchSimilarPages(
      Page page, boolean limit) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(
        PROPERTY_LIST,
        PROPERTY_LIST_SEARCH);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    if (page.getNamespace() != null) {
      properties.put(PROPERTY_NAMESPACE, page.getNamespace().toString());
    } else {
      properties.put(PROPERTY_NAMESPACE, "0");
    }
    properties.put(PROPERTY_PROP, PROPERTY_PROP_TITLESNIPPET);
    properties.put(PROPERTY_REDIRECTS, "true");
    properties.put(PROPERTY_SEARCH, "intitle:\"" + page.getTitle().replaceAll("\"", "\"\"") + "\"");
    List<Page> list = new ArrayList<Page>();
    int maxSize = getMaxSize(limit, ConfigurationValueInteger.MAX_SEARCH);
    while (result.executeSearch(properties, list) && (list.size() < maxSize)) {
      //
    }
    Collections.sort(list);
    page.setRelatedPages(Page.RelatedPages.SIMILAR_PAGES, list);
  }
}
