/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;


/**
 * MediaWiki API templates requests.
 */
public class ApiTemplatesRequest extends ApiPropertiesRequest {

  // ==========================================================================
  // API properties
  // ==========================================================================

  /**
   * Property for Name space.
   */
  public final static String PROPERTY_NAMESPACE = "tlnamespace";

  /**
   * Property for Limit.
   */
  public final static String PROPERTY_LIMIT = "tllimit";

  /**
   * Property for Templates.
   */
  public final static String PROPERTY_TEMPLATES = "tltemplates";

  /**
   * Maximum templates in a query.
   */
  public final static int MAX_TEMPLATES_PER_QUERY = 50;

  // ==========================================================================
  // Request management
  // ==========================================================================

  private final ApiTemplatesResult result;

  /**
   * @param wiki Wiki.
   * @param result Parser for result depending on chosen format.
   */
  public ApiTemplatesRequest(EnumWikipedia wiki, ApiTemplatesResult result) {
    super(wiki);
    this.result = result;
  }

  /**
   * Load list of templates.
   * 
   * @param page Page for which templates are requested.
   * @throws APIException Exception thrown by the API.
   */
  public void loadTemplates(Page page) throws APIException {
    Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
    properties.put(PROPERTY_PROP, PROPERTY_PROP_INFO);
    properties.put(PROPERTY_CONTINUE, PROPERTY_CONTINUE_DEFAULT);
    properties.put(PROPERTY_GENERATOR, PROPERTY_PROP_TEMPLATES);
    properties.put(GENERATOR_PREFIX + PROPERTY_LIMIT, LIMIT_MAX);
    properties.put(PROPERTY_TITLES, page.getTitle());
    List<Page> list = new ArrayList<Page>();
    while (result.executeTemplates(properties, page, list)) {
      //
    }
    Collections.sort(list);
    page.setTemplates(list);
  }

  /**
   * Set disambiguation status of a list of pages.
   * 
   * @param pages List of pages.
   * @throws APIException Exception thrown by the API.
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

    // Search disambiguation templates for pages in the main name space
    List<Collection<Page>> splitPagesList = splitListPages(pages, MAX_PAGES_PER_QUERY);
    for (Collection<Page> splitPages : splitPagesList) {
      for (Page page : splitPages) {
        Iterator<Page> itPage = page.getRedirects().getIteratorWithPage();
        while (itPage.hasNext()) {
          itPage.next().setDisambiguationPage(null);
        }
      }
      List<Collection<Page>> splitTemplatesList = splitListPages(
          getWiki().getDisambiguationTemplates(), MAX_TEMPLATES_PER_QUERY);
      for (Collection<Page> templates : splitTemplatesList) {
        Map<String, String> properties = getProperties(ACTION_QUERY, result.getFormat());
        properties.put(
            PROPERTY_PROP,
            PROPERTY_PROP_TEMPLATES);
        properties.put(PROPERTY_LIMIT, LIMIT_MAX);
        properties.put(PROPERTY_REDIRECTS, "");
        properties.put(
            PROPERTY_TEMPLATES,
            constructListTitles(templates));
        properties.put(PROPERTY_TITLES, constructListTitles(splitPages));
        while (result.setDiambiguationStatus(properties, splitPages)) {
          //
        }
      }
      for (Page page : splitPages) {
        Iterator<Page> itPage = page.getRedirects().getIteratorWithPage();
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
