/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiPropertiesResult;
import org.wikipediacleaner.api.request.ApiRequest;


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
   * @throws JDOMException
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
    page.setStartTimestamp(node.getAttributeValue("starttimestamp"));
    Attribute attrRedirect = node.getAttribute("redirect");
    if (attrRedirect != null) {
      page.isRedirect(true);
    }
    Attribute attrMissing = node.getAttribute("missing");
    if (attrMissing != null) {
      page.setExisting(Boolean.FALSE);
    }

    // Retrieve protection information
    XPath xpaProtection = XPath.newInstance("protection/pr[@type=\"edit\"]");
    Element protectionNode = (Element) xpaProtection.selectSingleNode(node);
    if (protectionNode != null) {
      XPath xpaLevel = XPath.newInstance("./@level");
      page.setEditProtectionLevel(xpaLevel.valueOf(protectionNode));
    }
  }

  /**
   * Execute redirect request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with redirect information.
   * @throws APIException
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
   * @param normalization Map containing information about title normalization (From => To).
   * @throws JDOMException
   */
  public void retrieveNormalization(
      Element root,
      Map<String, String> normalization) throws JDOMException {
    if (normalization == null) {
      return;
    }
    XPath xpaNormalized = XPath.newInstance("/api/query/normalized/n");
    List listNormalized = xpaNormalized.selectNodes(root);
    if ((listNormalized == null) || (listNormalized.isEmpty())) {
      return;
    }
    Iterator itNormalized = listNormalized.iterator();
    XPath xpaFrom = XPath.newInstance("./@from");
    XPath xpaTo = XPath.newInstance("./@to");
    while (itNormalized.hasNext()) {
      Element normalized = (Element) itNormalized.next();
      String from = xpaFrom.valueOf(normalized);
      String to = xpaTo.valueOf(normalized);
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
   * @throws JDOMException
   */
  public void updateRedirect(Element root, Collection<Page> pages) throws JDOMException {

    // Retrieving redirects
    XPath xpaRedirects = XPath.newInstance("/api/query/redirects/r");
    List listRedirects = xpaRedirects.selectNodes(root);
    XPath xpaFrom = XPath.newInstance("./@from");
    XPath xpaTo = XPath.newInstance("./@to");

    // Retrieving pages
    XPath xpaPages = XPath.newInstance("/api/query/pages");
    Element listPages = (Element) xpaPages.selectSingleNode(root);
    XPath xpaPageId = XPath.newInstance("./@pageid");
    XPath xpaNamespace = XPath.newInstance("./@ns");
    XPath xpaTitle = XPath.newInstance("./@title");

    // Retrieving normalization information
    Map<String, String> normalization = new HashMap<String, String>();
    retrieveNormalization(root, normalization);

    // Analyzing redirects
    Iterator itRedirect = listRedirects.iterator();
    while (itRedirect.hasNext()) {
      Element currentRedirect = (Element) itRedirect.next();
      String fromPage = xpaFrom.valueOf(currentRedirect);
      String toPage = xpaTo.valueOf(currentRedirect);
      for (Page p : pages) {

        // Find if the redirect is already taken into account
        boolean exists = false;
        Iterator<Page> itPage = p.getRedirectIteratorWithPage();
        while (itPage.hasNext()) {
          Page tmp = itPage.next();
          String title = getNormalizedTitle(tmp.getTitle(), normalization);
          if (Page.areSameTitle(title, toPage)) {
            exists = true;
          }
        }

        // Add the redirect if needed
        itPage = p.getRedirectIteratorWithPage();
        while (itPage.hasNext()) {
          Page tmp = itPage.next();
          String title = getNormalizedTitle(tmp.getTitle(), normalization);
          if (!exists && Page.areSameTitle(title, fromPage)) {
            XPath xpaPage = createXPath("page", "title", toPage);
            List listTo = xpaPage.selectNodes(listPages);
            if (!listTo.isEmpty()) {
              Element to = (Element) listTo.get(0);
              Page pageTo = DataManager.getPage(
                  p.getWikipedia(), xpaTitle.valueOf(to), null, null, null);
              pageTo.setNamespace(xpaNamespace.valueOf(to));
              pageTo.setPageId(xpaPageId.valueOf(to));
              p.addRedirect(pageTo);
            }
          }
        }
      }
    }

    // Analyzing missing pages
    for (Page p : pages) {
      Iterator<Page> itPage = p.getRedirectIteratorWithPage();
      while (itPage.hasNext()) {
        Page tmp = itPage.next();
        String title = getNormalizedTitle(tmp.getTitle(), normalization);
        XPath xpaPage = createXPath("page", "title", title);
        Element page = (Element) xpaPage.selectSingleNode(listPages);
        if (page != null) {
          List pageId = xpaPageId.selectNodes(page);
          if ((pageId != null) && (!pageId.isEmpty())) {
            tmp.setExisting(Boolean.TRUE);
          } else {
            Attribute attrMissing = page.getAttribute("missing");
            if (attrMissing != null) {
              tmp.setExisting(Boolean.FALSE);
            }
          }
        }
      }
    }
  }
}
