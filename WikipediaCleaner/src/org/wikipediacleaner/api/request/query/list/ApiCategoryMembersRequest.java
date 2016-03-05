/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * MediaWiki category members requests.
 */
public class ApiCategoryMembersRequest extends ApiListRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for direction.
   */
  public final static String PROPERTY_DIR = "cmdir";

  /**
   * Property value for direction / Ascending.
   */
  public final static String PROPERTY_DIR_ASC = "asc";

  /**
   * Property value for direction / Descending.
   */
  public final static String PROPERTY_DIR_DESC = "desc";

  /**
   * Property for End.
   */
  public final static String PROPERTY_END = "cmend";

  /**
   * Property for End sort key.
   */
  public final static String PROPERTY_ENDSORTKEY = "cmendsortkey";

  /**
   * Property for End sort key prefix.
   */
  public final static String PROPERTY_ENDSORTKEYPREFIX = "cmendsortkeyprefix";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "cmlimit";

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "cmnamespace";

  /**
   * Property for Page ID.
   */
  public final static String PROPERTY_PAGEID = "cmpageid";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROP = "cmprop";

  /**
   * Property value for Properties / Page ID.
   */
  public final static String PROPERTY_PROP_IDS = "ids";

  /**
   * Property value for Properties / Sort key.
   */
  public final static String PROPERTY_PROP_SORTKEY = "sortkey";

  /**
   * Property value for Properties / Sort key prefix.
   */
  public final static String PROPERTY_PROP_SORTKEYPREFIX = "sortkeyprefix";

  /**
   * Property value for Properties / Time stamp.
   */
  public final static String PROPERTY_PROP_TIMESTAMP = "timestamp";

  /**
   * Property value for Properties / Title.
   */
  public final static String PROPERTY_PROP_TITLE = "title";

  /**
   * Property value for Properties / Type.
   */
  public final static String PROPERTY_PROP_TYPE = "type";

  /**
   * Property for Sort.
   */
  public final static String PROPERTY_SORT = "cmsort";

  /**
   * Property value for Sort / Sort key.
   */
  public final static String PROPERTY_SORT_SORTKEY = "sortkey";

  /**
   * Property value for Sort / Time stamp.
   */
  public final static String PROPERTY_SORT_TIMESTAMP = "timestamp";

  /**
   * Property for Start.
   */
  public final static String PROPERTY_START = "cmstart";

  /**
   * Property for Start sort key.
   */
  public final static String PROPERTY_STARTSORTKEY = "cmstartsortkey";

  /**
   * Property for Start sort key prefix.
   */
  public final static String PROPERTY_STARTSORTKEYPREFIX = "cmstartsortkeyprefix";

  /**
   * Property for Title.
   */
  public final static String PROPERTY_TITLE = "cmtitle";

  /**
   * Property for Type.
   */
  public final static String PROPERTY_TYPE = "cmtype";

  /**
   * Property value for Type / File.
   */
  public final static String PROPERTY_TYPE_FILE = "file";

  /**
   * Property value for Type / Page.
   */
  public final static String PROPERTY_TYPE_PAGE = "page";

  /**
   * Property value for Type / Sub category.
   */
  public final static String PROPERTY_TYPE_SUBCAT = "subcat";

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiCategoryMembersResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiCategoryMembersRequest(EnumWikipedia wiki, ApiCategoryMembersResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of category members.
   * 
   * @param category Category for which members are requested.
   * @param depth Depth of lookup for sub-categories.
   * @param limit Flag indicating if the number of results should be limited.
   * @param max Absolute maximum number of results
   */
  public void loadCategoryMembers(
      Page category,
      int depth, boolean limit, int max) throws APIException {

    List<String> categoriesAnalyzed = new ArrayList<String>();
    Map<Page, Integer> categories = new HashMap<Page, Integer>();
    categories.put(category, Integer.valueOf(0));
    int maxSize = getMaxSize(limit, ConfigurationValueInteger.MAX_CATEGORY_MEMBERS);
    maxSize = Math.min(maxSize, max);
    while (!categories.isEmpty()) {

      // Find which category to analyze
      Entry<Page, Integer> entry = categories.entrySet().iterator().next();
      Page currentCategory = entry.getKey();
      categories.remove(currentCategory);
      int currentDepth = entry.getValue().intValue();
      String categoryName = currentCategory.getTitle();
      int colonIndex = categoryName.indexOf(':');
      if (colonIndex < 0) {
        categoryName = getWiki().getWikiConfiguration().getPageTitle(
            Namespace.CATEGORY, categoryName);
      } else {
        Namespace namespaceCategory = getWiki().getWikiConfiguration().getNamespace(Namespace.CATEGORY);
        if (!namespaceCategory.isPossibleName(categoryName.substring(0, colonIndex))) {
          categoryName = getWiki().getWikiConfiguration().getPageTitle(Namespace.CATEGORY, categoryName);
        }
      }
      boolean shouldAnalyze = true;
      if (currentDepth > depth) {
        shouldAnalyze = false;
      }
      if (categoriesAnalyzed.contains(categoryName)) {
        shouldAnalyze = false;
      }

      // Analyze the category
      if (shouldAnalyze) {
        categoriesAnalyzed.add(categoryName);
        Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
        properties.put(PROPERTY_LIST, PROPERTY_LIST_CATEGORYMEMBERS);
        properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
        properties.put(PROPERTY_LIMIT, LIMIT_MAX);
        properties.put(PROPERTY_TITLE, categoryName);
        List<Page> list = new ArrayList<Page>();
        while (result.executeCategoryMembers(
            properties, list, categories, currentDepth) &&
            (list.size() < maxSize)) {
          //
        }
        Collections.sort(list);
        currentCategory.setRelatedPages(Page.RelatedPages.CATEGORY_MEMBERS, list);
      }
    }
  }
}
