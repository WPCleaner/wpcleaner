/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

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
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML links here results.
 */
public class ApiXmlLinksHereResult extends ApiXmlResult implements ApiLinksHereResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLinksHereResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }


  /**
   * Execute links here request.
   * 
   * @param properties Properties defining request.
   * @param page Main page.
   * @param lists Lists to be filled with links to the page.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeLinksHere(
      Map<String, String> properties,
      Page page,
      Map<String, List<Page>> lists) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve links to page
      // TODO
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/pages/page", Filters.element());
      List<Element> listPages = xpa.evaluate(root);
      Iterator<Element> itPages = listPages.iterator();
      XPathExpression<Element> xpaLinksHere = XPathFactory.instance().compile(
          "./linkshere/lh", Filters.element());
      while (itPages.hasNext()) {
        Element currentPage = itPages.next();
        String title = currentPage.getAttributeValue("title");
        List<Page> list = lists.get(title);
        if (list == null) {
          list = new ArrayList<>();
          lists.put(title, list);
        }
        List<Element> listLinks = xpaLinksHere.evaluate(currentPage);
        Iterator<Element> itLinks = listLinks.iterator();
        while (itLinks.hasNext()) {
          Element currentLink = itLinks.next();
          Page link = DataManager.getPage(
              getWiki(), currentLink.getAttributeValue("title"),
              null, null, page.getRelatedPages(RelatedPages.REDIRECTS));
          link.setNamespace(currentLink.getAttributeValue("ns"));
          link.setPageId(currentLink.getAttributeValue("pageid"));
          if (currentLink.getAttribute("redirect") != null) {
            link.isRedirect(true);
          }
          if (!list.contains(link)) {
            list.add(link);
          }
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/linkshere",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading links here", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
