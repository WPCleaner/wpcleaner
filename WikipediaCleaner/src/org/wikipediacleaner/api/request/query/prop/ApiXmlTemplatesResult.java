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
 * MediaWiki API XML templates results.
 */
public class ApiXmlTemplatesResult extends ApiXmlPropertiesResult implements ApiTemplatesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlTemplatesResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute templates request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List of pages to be filled with the templates.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeTemplates(
      Map<String, String> properties,
      Page page,
      List<Page> list)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve back links
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/pages/page", Filters.element());
      List<Element> listTemplates = xpa.evaluate(root);
      Iterator<Element> itTemplate = listTemplates.iterator();
      while (itTemplate.hasNext()) {
        Element currentTemplate = itTemplate.next();
        String pageId = currentTemplate.getAttributeValue("pageid");
        String ns = currentTemplate.getAttributeValue("ns");
        String title = currentTemplate.getAttributeValue("title");
        Page template = DataManager.getPage(
            getWiki(), title, null, null, null);
        template.setNamespace(ns);
        template.setPageId(pageId);
        if (currentTemplate.getAttribute("missing") != null) {
          template.setExisting(Boolean.FALSE);
        }
        if (!list.contains(template)) {
          list.add(template);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/templates",
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
      XPathExpression<Element> xpaTemplates = XPathFactory.instance().compile(
          "templates/tl", Filters.element());
      List<Page> tmpPages = new ArrayList<>();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String title = currentNode.getAttributeValue("title");
        for (Page p : pages) {
          tmpPages.clear();
          Iterator<Page> it = p.getRedirects().getIteratorWithPage();
          while (it.hasNext()) {
            Page p2 = it.next();
            tmpPages.add(p2);
            if ((p2.getTitle() != null) &&
                (Page.areSameTitle(p2.getTitle(), title))) {
              List<Element> listTemplates = xpaTemplates.evaluate(currentNode);
              boolean hasTemplate = false;
              for (Element template : listTemplates) {
                if (!hasTemplate && ("" + Namespace.TEMPLATE).equals(template.getAttribute("ns").getValue())) {
                  hasTemplate = true;
                }
              }
              if (hasTemplate) {
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
          root, "/api/query-continue/templates",
          properties);
    } catch (JDOMException e) {
      log.error("Error updating disambiguation status", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
