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
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiRevisionsResult;
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * MediaWiki API XML revisions results.
 */
public class ApiXmlRevisionsResult extends ApiXmlPropertiesResult implements ApiRevisionsResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlRevisionsResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
  }

  /**
   * Execute last revision request.
   * 
   * @param properties Properties defining request.
   * @param pages Pages to be filled with last revision content.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeLastRevision(
      Map<String, String> properties,
      Collection<Page> pages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Manage redirects and missing pages
      updateRedirect(root, pages);

      // Retrieve pages
      XPath xpa = XPath.newInstance("/api/query/pages/page");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      while (iter.hasNext()) {
        Element pageNode = (Element) iter.next();
        String title = xpaTitle.valueOf(pageNode);
        String namespace = xpaNs.valueOf(pageNode);
        for (Page tmpPage : pages) {
          Iterator<Page> itPage = tmpPage.getRedirectIteratorWithPage();
          while (itPage.hasNext()) {
            Page page = itPage.next();
            if (Page.areSameTitle(page.getTitle(), title)) {
              page.setNamespace(namespace);
              updatePageInformation(pageNode, page);
  
              // Retrieve revisions
              XPath xpaRevisions = XPath.newInstance("revisions/rev");
              Element revNode = (Element) xpaRevisions.selectSingleNode(pageNode);
              if (revNode != null) {
                XPath xpaContents = XPath.newInstance(".");
                XPath xpaRevision = XPath.newInstance("./@revid");
                XPath xpaTimestamp = XPath.newInstance("./@timestamp");
                page.setContents(xpaContents.valueOf(revNode));
                page.setExisting(Boolean.TRUE);
                page.setRevisionId(xpaRevision.valueOf(revNode));
                page.setContentsTimestamp(xpaTimestamp.valueOf(revNode));
              }
            }
          }
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/revisions",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading revisions", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
