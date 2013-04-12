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
          link.setBackLinks(linkList);
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
