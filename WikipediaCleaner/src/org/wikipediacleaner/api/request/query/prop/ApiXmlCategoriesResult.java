/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.filter.Filters;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML categories results.
 */
public class ApiXmlCategoriesResult extends ApiXmlPropertiesResult implements ApiCategoriesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlCategoriesResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute categories request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List to be filled with categories.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeCategories(
      Map<String, String> properties,
      Page page,
      List<Page> list) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve back links
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/pages/page/categories/cl", Filters.element());
      List<Element> listCategories = xpa.evaluate(root);
      Iterator<Element> itCategory = listCategories.iterator();
      while (itCategory.hasNext()) {
        Element currentCategory = itCategory.next();
        String pageId = currentCategory.getAttributeValue("pageid");
        String ns = currentCategory.getAttributeValue("ns");
        String title = currentCategory.getAttributeValue("title");
        Page category = DataManager.getPage(
            getWiki(), title, null, null, null);
        category.setNamespace(ns);
        category.setPageId(pageId);
        if (currentCategory.getAttribute("missing") != null) {
          category.setExisting(Boolean.FALSE);
        }
        if (!list.contains(category)) {
          list.add(category);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/categories",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Set disambiguation status of a list of pages.
   * 
   * @param properties Properties defining request.
   * @param pages List of pages for which disambiguation status needs to be set.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean setDiambiguationStatus(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Manage redirects and missing pages
      updateRedirect(root, pages);

      // Set disambiguation status
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/pages/page", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      XPathExpression<Element> xpaCategory = XPathFactory.instance().compile(
          "categories/cl", Filters.element());
      List<Page> tmpPages = new ArrayList<Page>();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String title = currentNode.getAttributeValue("title");
        for (Page p : pages) {
          tmpPages.clear();
          Iterator<Page> it = p.getRedirectIteratorWithPage();
          while (it.hasNext()) {
            Page p2 = it.next();
            tmpPages.add(p2);
            if ((p2.getTitle() != null) &&
                (Page.areSameTitle(p2.getTitle(), title))) {
              List<Element> listCategories = xpaCategory.evaluate(currentNode);
              boolean dab = false;
              for (Element category : listCategories) {
                if (!dab && (category.getAttribute("ns").equals("" + Namespace.CATEGORY))) {
                  dab = true;
                }
              }
              if (dab) {
                for (Page p3 : tmpPages) {
                  p3.setDisambiguationPage(Boolean.TRUE);
                }
              }
            }
          }
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/categories",
          properties);
    } catch (JDOMException e) {
      log.error("Error updating disambiguation status", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
