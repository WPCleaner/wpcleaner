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
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiUsersResult;


/**
 * MediaWiki API XML users results.
 */
public class ApiXmlUsersResult extends ApiXmlResult implements ApiUsersResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlUsersResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute user request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public User executeUser(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Get recent changes list
      XPath xpa = XPath.newInstance("/api/query/users/user");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        User user = new User(currentNode.getAttributeValue("name"));
        List<String> groups = new ArrayList<String>();
        XPath xpaGroups = XPath.newInstance("./groups/g");
        List resultGroups = xpaGroups.selectNodes(currentNode);
        Iterator itGroups = resultGroups.iterator();
        while (itGroups.hasNext()) {
          groups.add(((Element) itGroups.next()).getValue());
        }
        user.setGroups(groups);
        List<String> rights = new ArrayList<String>();
        XPath xpaRights = XPath.newInstance("./rights/r");
        List resultRights = xpaRights.selectNodes(currentNode);
        Iterator itRights = resultRights.iterator();
        while (itRights.hasNext()) {
          rights.add(((Element) itRights.next()).getValue());
        }
        user.setRights(rights);
        return user;
      }
    } catch (JDOMException e) {
      log.error("Error retrieving user information", e);
      throw new APIException("Error parsing XML", e);
    }
    return null;
  }
}
