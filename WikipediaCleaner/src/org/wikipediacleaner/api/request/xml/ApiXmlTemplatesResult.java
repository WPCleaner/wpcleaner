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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiTemplatesResult;
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * MediaWiki API XML templates results.
 */
public class ApiXmlTemplatesResult extends ApiXmlPropertiesResult implements ApiTemplatesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlTemplatesResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
  }

  /**
   * Set disambiguation status of a list of pages.
   * 
   * @param properties Properties defining request.
   * @param pages List of pages for which disambiguation status needs to be set.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean setDiambiguationStatus(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Manage redirects and missing pages
      updateRedirect(root, pages);

      // Set disambiguation status
      XPath xpa = XPath.newInstance("/api/query/pages/page");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaTitle = XPath.newInstance("./@title");
      XPath xpaTemplate = createXPath("templates/tl", "ns", "" + Namespace.TEMPLATE);
      List<Page> tmpPages = new ArrayList<Page>();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String title = xpaTitle.valueOf(currentNode);
        for (Page p : pages) {
          tmpPages.clear();
          Iterator<Page> it = p.getRedirectIteratorWithPage();
          while (it.hasNext()) {
            Page p2 = it.next();
            tmpPages.add(p2);
            if ((p2.getTitle() != null) &&
                (p2.getTitle().equals(title))) {
              List listTemplates = xpaTemplate.selectNodes(currentNode);
              if (listTemplates.size() > 0) {
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
