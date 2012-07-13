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
import org.jdom.input.JDOMParseException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiQueryListSearchResult;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * Class for MediaWiki API XML search results.
 */
public class ApiXmlQueryListSearchResult extends ApiXmlResult implements ApiQueryListSearchResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlQueryListSearchResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
  }

  /**
   * Execute search request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with back links.
   * @return Value for continuing request if needed.
   * @throws APIException
   */
  public String executeSearch(
      Map<String, String> properties,
      List<Page> list)
          throws APIException {
    try {
      return constructSearchResults(
          getRoot(properties, ApiRequest.MAX_ATTEMPTS),
          list);
    } catch (JDOMParseException e) {
      log.error("Error searching", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Construct list of search results.
   * 
   * @param root Root element.
   * @param list List of pages to be filled with back links.
   * @return Value for continuing request if needed.
   * @throws APIException
   */
  private String constructSearchResults(
      Element root,
      List<Page> list)
      throws APIException {

    // Retrieve back links
    try {
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
    } catch (JDOMException e) {
      log.error("Error search", e);
      throw new APIException("Error parsing XML result", e);
    }

    // Retrieve continue
    try {
      XPath xpa = XPath.newInstance("/api/query-continue/backlinks");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaContinue = XPath.newInstance("./@blcontinue");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        return xpaContinue.valueOf(currentNode);
      }
    } catch (JDOMException e) {
      log.error("Error watchlist raw", e);
      throw new APIException("Error parsing XML result", e);
    }
    return null;
  }
}
