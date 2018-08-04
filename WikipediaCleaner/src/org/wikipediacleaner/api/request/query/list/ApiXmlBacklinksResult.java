/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML back links results.
 */
@Deprecated
public class ApiXmlBacklinksResult extends ApiXmlResult implements ApiBacklinksResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlBacklinksResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute back links request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List of pages to be filled with the back links.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeBacklinks(
      Map<String, String> properties,
      Page page,
      List<Page> list)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve back links
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/backlinks/bl", Filters.element());
      List<Element> listBacklinks = xpa.evaluate(root);
      Iterator<Element> itBacklink = listBacklinks.iterator();
      XPathExpression<Element> xpaRedirLinks = XPathFactory.instance().compile(
          "redirlinks/bl", Filters.element());
      while (itBacklink.hasNext()) {
        Element currentBacklink = itBacklink.next();
        Page link = DataManager.getPage(
            getWiki(), currentBacklink.getAttributeValue("title"), null, null, null);
        link.setNamespace(currentBacklink.getAttributeValue("ns"));
        link.setPageId(currentBacklink.getAttributeValue("pageid"));
        if (currentBacklink.getAttribute("redirect") != null) {
          link.addRedirect(page);
        }
        if (!list.contains(link)) {
          list.add(link);
        }

        // Links through redirects
        List<Element> listRedirLinks = xpaRedirLinks.evaluate(currentBacklink);
        if (listRedirLinks != null) {
          List<Page> linkList = new ArrayList<Page>();
          Iterator<Element> itRedirLink = listRedirLinks.iterator();
          while (itRedirLink.hasNext()) {
            currentBacklink = itRedirLink.next();
            Page link2 = DataManager.getPage(
                getWiki(), currentBacklink.getAttributeValue("title"), null, null, null);
            link2.setNamespace(currentBacklink.getAttributeValue("ns"));
            link2.setPageId(currentBacklink.getAttributeValue("pageid"));
            if (!list.contains(link2)) {
              list.add(link2);
            }
            if (!linkList.contains(link2)) {
              linkList.add(link2);
            }
          }
          link.setRelatedPages(Page.RelatedPages.BACKLINKS, linkList);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/backlinks",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading back links", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
