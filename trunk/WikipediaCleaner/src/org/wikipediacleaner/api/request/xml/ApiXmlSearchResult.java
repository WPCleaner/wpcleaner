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
import org.wikipediacleaner.api.request.ApiSearchResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * Class for MediaWiki API XML search results.
 */
public class ApiXmlSearchResult extends ApiXmlResult implements ApiSearchResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlSearchResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute search request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with back links.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeSearch(
      Map<String, String> properties,
      List<Page> list)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve search results
      XPath xpa = XPath.newInstance("/api/query/search/p");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Page similarPage = DataManager.getPage(
            getWiki(), xpaTitle.valueOf(currentNode), null, null);
        similarPage.setNamespace(xpaNs.valueOf(currentNode));
        list.add(similarPage);
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/search",
          properties);
    } catch (JDOMException e) {
      log.error("Error searching", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
