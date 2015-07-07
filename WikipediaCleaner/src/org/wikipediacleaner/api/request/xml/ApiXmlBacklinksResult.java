/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

import java.util.ArrayList;
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
import org.wikipediacleaner.api.request.ApiBacklinksResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML back links results.
 */
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
   * @throws APIException
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
      XPath xpa = XPath.newInstance("/api/query/backlinks/bl");
      List listBacklinks = xpa.selectNodes(root);
      Iterator itBacklink = listBacklinks.iterator();
      XPath xpaPageId = XPath.newInstance("./@pageid");
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      XPath xpaRedirLinks = XPath.newInstance("redirlinks/bl");
      while (itBacklink.hasNext()) {
        Element currentBacklink = (Element) itBacklink.next();
        Page link = DataManager.getPage(
            getWiki(), xpaTitle.valueOf(currentBacklink), null, null, null);
        link.setNamespace(xpaNs.valueOf(currentBacklink));
        link.setPageId(xpaPageId.valueOf(currentBacklink));
        if (currentBacklink.getAttribute("redirect") != null) {
          link.addRedirect(page);
        }
        if (!list.contains(link)) {
          list.add(link);
        }

        // Links through redirects
        List listRedirLinks = xpaRedirLinks.selectNodes(currentBacklink);
        if (listRedirLinks != null) {
          List<Page> linkList = new ArrayList<Page>();
          Iterator itRedirLink = listRedirLinks.iterator();
          while (itRedirLink.hasNext()) {
            currentBacklink = (Element) itRedirLink.next();
            Page link2 = DataManager.getPage(
                getWiki(), xpaTitle.valueOf(currentBacklink), null, null, null);
            link2.setNamespace(xpaNs.valueOf(currentBacklink));
            link2.setPageId(xpaPageId.valueOf(currentBacklink));
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
