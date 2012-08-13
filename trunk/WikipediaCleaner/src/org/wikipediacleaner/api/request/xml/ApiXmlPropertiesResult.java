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

package org.wikipediacleaner.api.request.xml;

import java.util.Collection;
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
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * MediaWiki API XML results for properties.
 */
public class ApiXmlPropertiesResult extends ApiXmlResult implements ApiPropertiesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlPropertiesResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
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
    Attribute attrEditToken = node.getAttribute("edittoken");
    if (attrEditToken != null) {
      page.setEditToken(attrEditToken.getValue());
    }
    XPath xpaPageId = XPath.newInstance("./@pageid");
    page.setPageId(xpaPageId.valueOf(node));
    XPath xpaStartTimestamp = XPath.newInstance("./@starttimestamp");
    page.setStartTimestamp(xpaStartTimestamp.valueOf(node));
    Attribute attrRedirect = node.getAttribute("redirect");
    if (attrRedirect != null) {
      page.isRedirect(true);
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
          if ((tmp.getTitle() != null) && (tmp.getTitle().equals(toPage))) {
            exists = true;
          }
        }

        // Add the redirect if needed
        itPage = p.getRedirectIteratorWithPage();
        while (itPage.hasNext()) {
          Page tmp = itPage.next();
          if (!exists &&
              (tmp.getTitle() != null) &&
              (tmp.getTitle().equals(fromPage))) {
            XPath xpaPage = createXPath("page", "title", toPage);
            List listTo = xpaPage.selectNodes(listPages);
            if (!listTo.isEmpty()) {
              Element to = (Element) listTo.get(0);
              Page pageTo = DataManager.getPage(
                  p.getWikipedia(), xpaTitle.valueOf(to), null, null);
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
        XPath xpaPage = createXPath("page", "title", tmp.getTitle());
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
