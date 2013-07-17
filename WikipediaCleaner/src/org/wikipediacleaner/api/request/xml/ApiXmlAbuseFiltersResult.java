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
import org.wikipediacleaner.api.data.AbuseFilter;
import org.wikipediacleaner.api.request.ApiAbuseFiltersResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML abuse filters results.
 */
public class ApiXmlAbuseFiltersResult extends ApiXmlResult implements ApiAbuseFiltersResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlAbuseFiltersResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute abuse filters request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with abuse filters.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executeAbuseFilters(
      Map<String, String> properties,
      List<AbuseFilter> list) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve category members
      XPath xpa = XPath.newInstance("/api/query/abusefilters/filter");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Integer id = Integer.valueOf(0);
        try {
          String tmp = currentNode.getAttributeValue("id");
          if (tmp != null) {
            id = Integer.parseInt(tmp);
          }
        } catch (NumberFormatException e) {
          //
        }
        String description = currentNode.getAttributeValue("description");
        AbuseFilter filter = new AbuseFilter(id, description);
        filter.setDeleted(currentNode.getAttribute("deleted") != null);
        filter.setEnabled(currentNode.getAttribute("enabled") != null);
        list.add(filter);
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/abusefilters",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading abuse filters list", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
