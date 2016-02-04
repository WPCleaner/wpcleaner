/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;


/**
 * Utility class for managing search engines.
 */
public class SearchEngine {

  /** Search engine name */
  private final String name;

  /** URL for searching */
  private final String url;

  /**
   * @param name Search engine name.
   * @param url URL for searching.
   */
  private SearchEngine(String name, String url) {
    this.name = name;
    this.url = url;
  }

  /**
   * @return Search engine name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return URL for searching.
   */
  public String getUrl() {
    return url;
  }

  /**
   * @param wiki Wiki.
   * @param template Template.
   * @param property Specific property (if null, only general property will be used).
   * @return
   */
  public static Map<String, List<SearchEngine>> getSearchEngines(
      EnumWikipedia wiki, PageElementTemplate template,
      WPCConfigurationStringList property) {
    if ((wiki == null) || (template == null)) {
      return null;
    }
    Map<String, List<SearchEngine>> result = new HashMap<>();
    WPCConfiguration config = wiki.getConfiguration();
    List<String[]> searchEngines = null;

    // Retrieve search engines for specific property
    if (property != null) {
      searchEngines = config.getStringArrayList(property);
      populateSearchEnginesMap(result, searchEngines, template);
    }

    // Retrieve search engines for general property
    searchEngines = config.getStringArrayList(
        WPCConfigurationStringList.SEARCH_ENGINES_TEMPLATES);
    populateSearchEnginesMap(result, searchEngines, template);

    return result;
  }

  /**
   * @param map Map of search engines (parameter name => search engines)
   * @param searchEngines Search engines configuration
   * @param template
   */
  private static void populateSearchEnginesMap(
      Map<String, List<SearchEngine>> map,
      List<String[]> searchEngines,
      PageElementTemplate template) {
    if ((map == null) || (searchEngines == null) || (template == null)) {
      return;
    }

    try {
      for (String[] searchEngine : searchEngines) {
        if ((searchEngine.length >= 4) &&
            (Page.areSameTitle(template.getTemplateName(), searchEngine[2]))) {
          String[] parameterNames = searchEngine[3].split("\\,");
          for (String parameterName : parameterNames) {
            String value = template.getParameterValue(parameterName);
            if ((value != null) && (value.trim().length() > 0)) {
              List<SearchEngine> result = map.get(parameterName);
              if (result == null) {
                result = new ArrayList<>();
                map.put(parameterName, result);
              }
              String engineName = searchEngine[0].trim();
              String url = MessageFormat.format(
                  searchEngine[1].trim(), URLEncoder.encode(value, "UTF8"));
              result.add(new SearchEngine(engineName, url));
            }
          }
        }
      }
    } catch (UnsupportedEncodingException e) {
      // Nothing to do
    }
  }
}
