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
import org.wikipediacleaner.api.request.ApiLanguageLinksResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML templates results.
 */
public class ApiXmlLanguageLinksResult extends ApiXmlPropertiesResult implements ApiLanguageLinksResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLanguageLinksResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Get language links of a page.
   * 
   * @param properties Properties defining request.
   * @param languageLinks Map of language links to be set.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean getLanguageLinks(
      Map<String, String> properties,
      Map<String, String> languageLinks) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Set disambiguation status
      XPath xpa = XPath.newInstance("/api/query/pages/page/langlinks/ll");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaTitle = XPath.newInstance(".");
      XPath xpaLang = XPath.newInstance("./@lang");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String title = xpaTitle.valueOf(currentNode);
        String lang = xpaLang.valueOf(currentNode);
        if ((title != null) && (title.trim().length() > 0)) {
          languageLinks.put(lang, title);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/langlinks",
          properties);
    } catch (JDOMException e) {
      log.error("Error updating disambiguation status", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
