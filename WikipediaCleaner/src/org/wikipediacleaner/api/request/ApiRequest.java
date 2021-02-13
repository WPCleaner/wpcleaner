/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Base class for MediaWiki API requests.
 */
public abstract class ApiRequest {

  // ==========================================================================
  // API management
  // ==========================================================================

  /**
   * Maximum number of attempts for a request.
   */
  public final static int MAX_ATTEMPTS = 2;

  // ==========================================================================
  // API actions
  // ==========================================================================

  /**
   * API action.
   */
  public final static String ACTION = "action";

  /**
   * API action for deleting.
   */
  public final static String ACTION_DELETE = "delete";

  /**
   * API action for editing.
   */
  public final static String ACTION_EDIT = "edit";

  /**
   * API action for expanding templates.
   */
  public final static String ACTION_EXPAND = "expandtemplates";

  /**
   * API action for login.
   */
  public final static String ACTION_LOGIN = "login";

  /**
   * API action for logout.
   */
  public final static String ACTION_LOGOUT = "logout";

  /**
   * API action for parsing.
   */
  public final static String ACTION_PARSE = "parse";

  /**
   * API action for purging.
   */
  public final static String ACTION_PURGE = "purge";

  /**
   * API action for querying.
   */
  public final static String ACTION_QUERY = "query";

  /**
   * API action for retrieving TemplateData.
   */
  public final static String ACTION_TEMPLATE_DATA = "templatedata";

  /**
   * API action for retrieving tokens.
   */
  public final static String ACTION_TOKENS = "tokens";

  // ==========================================================================
  // API formats
  // ==========================================================================

  /**
   * API format.
   */
  public final static String FORMAT = "format";

  /**
   * API JSON format.
   */
  public final static String FORMAT_JSON = "json";

  /**
   * API XML format.
   */
  public final static String FORMAT_XML = "xml";

  // ==========================================================================
  // Limits
  // ==========================================================================

  /**
   * Limit set to maximum.
   */
  public final static String LIMIT_MAX = "max";

  /**
   * Maximum number of pages in a request.
   */
  public final static int MAX_PAGES_PER_QUERY = 50;

  /**
   * Maximum size for an URL encoded list.
   */
  public final static int MAX_LENGTH_LIST_URLENCODED = 1500;

  // ==========================================================================
  // Various parameters
  // ==========================================================================

  /**
   * Continuation mechanism.
   */
  public final static String PROPERTY_CONTINUE = "continue";

  /**
   * Continuation mechanism.
   */
  public final static String PROPERTY_CONTINUE_DEFAULT = "";

  /** Current timestamp */
  public final static String PROPERTY_CURRENT_TIMESTAMP = "curtimestamp";

  /** Value to include current timestamp in the response */
  public final static String PROPERTY_CURRENT_TIMESTAMP_YES = "1";

  // ==========================================================================
  // Wiki management
  // ==========================================================================

  private final EnumWikipedia wiki;

  /**
   * Base constructor.
   * 
   * @param wiki Wiki.
   */
  protected ApiRequest(EnumWikipedia wiki) {
    this.wiki = wiki;
  }

  /**
   * @return Wiki.
   */
  protected EnumWikipedia getWiki() {
    return wiki;
  }

  // ==========================================================================
  // Request management
  // ==========================================================================

  /**
   * Initialize a set of properties.
   * 
   * @param action Action called in the MediaWiki API.
   * @param format Format of the answer.
   * @return Properties.
   */
  protected Map<String, String> getProperties(String action, String format) {
    Map<String, String> properties = new HashMap<>();
    properties.put(ACTION, action);
    properties.put(FORMAT, format);
    return properties;
  }

  /**
   * Split a list of pages in smaller lists.
   * 
   * @param pages Full list of pages.
   * @param maxSize Maximum size for the resulting lists.
   * @return Lists of pages.
   */
  protected List<Collection<Page>> splitListPages(Collection<Page> pages, int maxSize) {
    if (pages == null) {
      return null;
    }
    List<Collection<Page>> result = new ArrayList<>();
    List<Page> currentList = new ArrayList<>();
    int pagesCount = 0;
    int charactersCount = 0;
    for (Page page : pages) {
      int length = 0;
      try {
        length = URLEncoder.encode(page.getTitle(), "UTF8").length();
      } catch (UnsupportedEncodingException e) {
        // Not supposed to happen.
      }
      if ((pagesCount + 1> maxSize) ||
          ((charactersCount + length > MAX_LENGTH_LIST_URLENCODED) && (pagesCount > 0))) {
        result.add(currentList);
        currentList = new ArrayList<>();
        pagesCount = 0;
        charactersCount = 0;
      }
      currentList.add(page);
      pagesCount++;
      charactersCount += length;
    }
    if (!currentList.isEmpty()) {
      result.add(currentList);
    }
    return result;
  }

  /**
   * Construct a textual representation of a list of pages.
   * 
   * @param pages List of pages.
   * @return Textual representation of the list.
   */
  protected String constructListTitles(Collection<Page> pages) {
    StringBuilder buffer = new StringBuilder();
    for (Page page : pages) {
      if (buffer.length() > 0) {
        buffer.append("|");
      }
      buffer.append(page.getTitle());
    }
    return buffer.toString();
  }

  /**
   * Construct a textual representation of a list of pages.
   * 
   * @param pages List of pages.
   * @return Textual representation of the list.
   */
  protected String constructListIds(Collection<Page> pages) {
    StringBuilder buffer = new StringBuilder();
    for (Page page : pages) {
      if (page.getPageId() != null) {
        if (buffer.length() > 0) {
          buffer.append("|");
        }
        buffer.append(page.getPageId());
      }
    }
    return buffer.toString();
  }

  /**
   * Construct a textual representation of a list of objects.
   * 
   * @param values List of objects.
   * @return Textual representation of the list.
   */
  protected String constructList(Collection<?> values) {
    StringBuilder buffer = new StringBuilder();
    for (Object value : values) {
      if (buffer.length() > 0) {
        buffer.append("|");
      }
      buffer.append(value.toString());
    }
    return buffer.toString();
  }

  /**
   * Get the maximum size authorized.
   * 
   * @param limit Flag indicating if the number of results should be limited.
   * @param property Property for the maximum size.
   * @return Maximum size authorized.
   */
  protected int getMaxSize(boolean limit, ConfigurationValueInteger property) {
    int maxSize = Integer.MAX_VALUE;
    if (limit) {
      Configuration config = Configuration.getConfiguration();
      maxSize = config.getInt(null, property);
    }
    return maxSize;
  }
}
