/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.request;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


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
            constructListPages(categories));
        properties.put(PROPERTY_TITLES, constructListPages(splitPages));
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
