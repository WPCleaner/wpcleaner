/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;


/**
 * MediaWiki API categories requests.
 */
public class ApiCategoriesRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Categories.
   */
  public final static String PROPERTY_CATEGORIES = "clcategories";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "cllimit";

  /**
   * Property for Properties.
   */
  public final static String PROPERTY_PROPERTIES = "clprop";

  /**
   * Property value for Properties / Sort key.
   */
  public final static String PROPERTY_PROPERTIES_SORTKEY = "sortkey";

  /**
   * Property value for Properties / Time stamp.
   */
  public final static String PROPERTY_PROPERTIES_TIMESTAMP = "timestamp";

  /**
   * Property for Show.
   */
  public final static String PROPERTY_SHOW = "clshow";

  /**
   * Property value for Show / Hidden.
   */
  public final static String PROPERTY_PROP_HIDDEN = "hidden";

  /**
   * Property value for Show / Not hidden.
   */
  public final static String PROPERTY_PROP_NOTHIDDEN = "!hidden";

  /**
   * Maximum categories in a query.
   */
  public final static int MAX_CATEGORIES_PER_QUERY = 50;

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiCategoriesResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiCategoriesRequest(EnumWikipedia wiki, ApiCategoriesResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Retrieve the categories of a page.
   * 
   * @param page Page.
   * @throws APIException
   */
  public void retrieveCategories(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_PROP, PROPERTY_PROP_CATEGORIES);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_TITLES, page.getTitle());
    List<Page> list = new ArrayList<Page>();
    while (result.executeCategories(properties, page, list)) {
      //
    }
    Collections.sort(list);
    page.setRelatedPages(RelatedPages.CATEGORIES, list);
  }

  /**
   * Set disambiguation status of a list of pages.
   * 
   * @param pages List of pages.
   */
  public void setDisambiguationStatus(Collection<Page> pages) throws APIException {

    // Check for pages outside the main name space
    List<Page> tmpPages = new ArrayList<Page>();
    for (Page page : pages) {
      if (page.isInMainNamespace()) {
        if (!tmpPages.contains(page)) {
          tmpPages.add(page);
        }
      } else {
        page.setDisambiguationPage(Boolean.FALSE);
      }
    }

    // Search disambiguation categories for pages in the main name space
    List<Collection<Page>> splitPagesList = splitListPages(pages, MAX_PAGES_PER_QUERY);
    for (Collection<Page> splitPages : splitPagesList) {
      for (Page page : splitPages) {
        Iterator<Page> itPage = page.getRedirectIteratorWithPage();
        while (itPage.hasNext()) {
          itPage.next().setDisambiguationPage(null);
        }
      }
      List<Collection<Page>> splitCategoriesList = splitListPages(
          getWiki().getConfiguration().getDisambiguationCategories(),
          MAX_CATEGORIES_PER_QUERY);
      for (Collection<Page> categories : splitCategoriesList) {
        Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
        properties.put(
            PROPERTY_PROP,
            PROPERTY_PROP_CATEGORIES);
        properties.put(PROPERTY_LIMIT, LIMIT_MAX);
        properties.put(PROPERTY_REDIRECTS, "");
        properties.put(
            PROPERTY_CATEGORIES,
            constructListTitles(categories));
        properties.put(PROPERTY_TITLES, constructListTitles(splitPages));
        while (result.setDiambiguationStatus(properties, splitPages)) {
          //
        }
      }
      for (Page page : splitPages) {
        Iterator<Page> itPage = page.getRedirectIteratorWithPage();
        while (itPage.hasNext()) {
          Page tmpPage = itPage.next();
          if (tmpPage.isDisambiguationPage() == null) {
            tmpPage.setDisambiguationPage(Boolean.FALSE);
          }
        }
      }
    }
  }
}
