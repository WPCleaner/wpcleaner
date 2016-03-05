/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

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
import org.wikipediacleaner.api.request.ApiXmlResult;


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
  @Override
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
