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
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
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
   * @param pages List of pages.
   * @param lists Lists to be filled with links to the page.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeLinksHere(
      Map<String, String> properties,
      Collection<Page> pages,
      Map<String, List<Page>> lists) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve links to page
      // TODO
      XPath xpa = XPath.newInstance("/api/query/pages/page");
      List listPages = xpa.selectNodes(root);
      Iterator itPages = listPages.iterator();
      XPath xpaPageId = XPath.newInstance("./@pageid");
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      XPath xpaLinksHere = XPath.newInstance("./linkshere/lh");
      while (itPages.hasNext()) {
        Element currentPage = (Element) itPages.next();
        String title = xpaTitle.valueOf(currentPage);
        List<Page> list = lists.get(title);
        if (list == null) {
          list = new ArrayList<>();
          lists.put(title, list);
        }
        List listLinks = xpaLinksHere.selectNodes(currentPage);
        Iterator itLinks = listLinks.iterator();
        while (itLinks.hasNext()) {
          Element currentLink = (Element) itLinks.next();
          Page link = DataManager.getPage(
              getWiki(), xpaTitle.valueOf(currentLink), null, null, null);
          link.setNamespace(xpaNs.valueOf(currentLink));
          link.setPageId(xpaPageId.valueOf(currentLink));
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
