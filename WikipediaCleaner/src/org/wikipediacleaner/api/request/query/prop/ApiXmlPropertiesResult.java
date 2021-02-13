/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Attribute;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.filter.Filters;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML results for properties.
 */
public class ApiXmlPropertiesResult extends ApiXmlResult implements ApiPropertiesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlPropertiesResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Update page information.
   * 
   * @param node Element for the page.
   * @param page Page.
   * @throws JDOMException Exception from due to the DOM.
   */
  public void updatePageInformation(Element node, Page page) throws JDOMException {

    // Retrieve basic page information
    Attribute attrPageId = node.getAttribute("pageid");
    if (attrPageId != null) {
      page.setPageId(attrPageId.getValue());
    }
    Attribute attrTitle = node.getAttribute("title");
    if (attrTitle != null) {
      page.setTitle(attrTitle.getValue());
    }
    Optional.ofNullable(node.getAttributeValue("starttimestamp")).ifPresent(timestamp -> page.setStartTimestamp(timestamp));
    Attribute attrRedirect = node.getAttribute("redirect");
    if (attrRedirect != null) {
      page.getRedirects().isRedirect(true);
    }
    Attribute attrMissing = node.getAttribute("missing");
    if (attrMissing != null) {
      page.setExisting(Boolean.FALSE);
    }

    // Retrieve protection information
    XPathExpression<Element> xpaProtection = XPathFactory.instance().compile(
        "protection/pr[@type=\"edit\"]", Filters.element());
    List<Element> protectionNodes = xpaProtection.evaluate(node);
    for (Element protectionNode : protectionNodes) {
      if ("edit".equals(protectionNode.getAttributeValue("type"))) {
        page.setEditProtectionLevel(protectionNode.getAttributeValue("level"));
      }
    }
  }

  /**
   * Execute redirect request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with redirect information.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void executeRedirect(
      Map<String, String> properties,
      List<Page> pages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
  
      // Manage redirects and missing pages
      updateRedirect(root, pages);
    } catch (JDOMException e) {
      log.error("Error loading redirects", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Retrieve information about page title normalization.
   * 
   * @param root Root element.
   * @param normalization Map containing information about title normalization (key=From, value=To).
   * @throws JDOMException Exception thrown due to the DOM.
   */
  public void retrieveNormalization(
      Element root,
      Map<String, String> normalization) throws JDOMException {
    if (normalization == null) {
      return;
    }
    XPathExpression<Element> xpaNormalized = XPathFactory.instance().compile(
        "/api/query/normalized/n", Filters.element());
    List<Element> listNormalized = xpaNormalized.evaluate(root);
    if ((listNormalized == null) || (listNormalized.isEmpty())) {
      return;
    }
    Iterator<Element> itNormalized = listNormalized.iterator();
    while (itNormalized.hasNext()) {
      Element normalized = itNormalized.next();
      String from = normalized.getAttributeValue("from");
      String to = normalized.getAttributeValue("to");
      if ((from != null) && (to != null)) {
        normalization.put(from, to);
      }
    }
  }

  /**
   * Retrieve the normalized title of a page.
   * 
   * @param title Title.
   * @param normalization Normalization information.
   * @return Normalized title.
   */
  public String getNormalizedTitle(String title, Map<String, String> normalization) {
    if ((title == null) || (normalization == null)) {
      return title;
    }
    String tmp = normalization.get(title);
    if (tmp != null) {
      return tmp;
    }
    return title;
  }

  /**
   * Update redirect and missing information of a list of pages.
   * 
   * @param root Root element.
   * @param pages List of pages.
   * @throws JDOMException Exception thrown due to the DOM.
   */
  public void updateRedirect(Element root, Collection<Page> pages) throws JDOMException {

    // Retrieving redirects
    XPathExpression<Element> xpaRedirects = XPathFactory.instance().compile(
        "/api/query/redirects/r", Filters.element());
    List<Element> listRedirects = xpaRedirects.evaluate(root);

    // Retrieving pages
    XPathExpression<Element> xpaPages = XPathFactory.instance().compile(
        "/api/query/pages/page", Filters.element());
    List<Element> listPages = xpaPages.evaluate(root);

    // Retrieving normalization information
    Map<String, String> normalization = new HashMap<>();
    retrieveNormalization(root, normalization);

    // Analyzing redirects
    Iterator<Element> itRedirect = listRedirects.iterator();
    while (itRedirect.hasNext()) {
      Element currentRedirect = itRedirect.next();
      String fromPage = currentRedirect.getAttributeValue("from");
      String toPage = currentRedirect.getAttributeValue("to");
      String toFragement = currentRedirect.getAttributeValue("tofragment");
      for (Page p : pages) {

        // Find if the redirect is already taken into account
        boolean exists = false;
        Iterator<Page> itPage = p.getRedirects().getIteratorWithPage();
        while (itPage.hasNext()) {
          Page tmp = itPage.next();
          String title = getNormalizedTitle(tmp.getTitle(), normalization);
          if (Page.areSameTitle(title, toPage)) {
            exists = true;
          }
        }

        // Add the redirect if needed
        itPage = p.getRedirects().getIteratorWithPage();
        while (itPage.hasNext()) {
          Page tmp = itPage.next();
          String title = getNormalizedTitle(tmp.getTitle(), normalization);
          if (!exists && Page.areSameTitle(title, fromPage)) {
            Element to = null;
            for (Element page : listPages) {
              if ((to == null) &&
                  Page.areSameTitle(toPage, page.getAttributeValue("title"))) {
                to = page;
              }
            }
            if (to != null) {
              Page pageTo = DataManager.getPage(
                  p.getWikipedia(), to.getAttributeValue("title"), null, null, null);
              pageTo.setNamespace(to.getAttributeValue("ns"));
              pageTo.setPageId(to.getAttributeValue("pageid"));
              p.getRedirects().add(pageTo, toFragement);
            }
          }
        }
      }
    }

    // Retrieve extra information about the pages
    XPathExpression<Element> xpaEditErrors = XPathFactory.instance().compile(
        "actions/edit/error", Filters.element());
    for (Page p : pages) {
      Iterator<Page> itPage = p.getRedirects().getIteratorWithPage();
      while (itPage.hasNext()) {
        Page tmp = itPage.next();
        String title = getNormalizedTitle(tmp.getTitle(), normalization);
        Element page = null;
        for (Element tmpPage : listPages) {
          if ((page == null) && title.equals(tmpPage.getAttributeValue("title"))) {
            page = tmpPage;
          }
        }
        if (page != null) {

          // Add information about missing pages
          if (page.getAttributeValue("pageid") != null) {
            tmp.setExisting(Boolean.TRUE);
          } else {
            Attribute attrMissing = page.getAttribute("missing");
            if (attrMissing != null) {
              tmp.setExisting(Boolean.FALSE);
            }
          }

          // Add information about translated pages
          List<Element> errorNodes = xpaEditErrors.evaluate(page);
          if ((errorNodes != null) && !errorNodes.isEmpty()) {
            tmp.setEditProhibition(true);
          }
        }
      }
    }
  }
}
