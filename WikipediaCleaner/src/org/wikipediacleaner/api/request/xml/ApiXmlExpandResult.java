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

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.request.ApiExpandResult;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * MediaWiki API XML expand results.
 */
public class ApiXmlExpandResult extends ApiXmlResult implements ApiExpandResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlExpandResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
  }

  /**
   * Execute expand templates request.
   * 
   * @param properties Properties defining request.
   * @return Expanded text.
   * @throws APIException
   */
  public String executeExpandTemplates(
      Map<String, String> properties)
          throws APIException {
    try {
      XPath xpaContents = XPath.newInstance("/api/expandtemplates/.");
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
      return xpaContents.valueOf(root);
    } catch (JDOMException e) {
      log.error("Error expanding templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
