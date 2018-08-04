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
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.filter.Filters;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
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
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public User executeUser(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Get recent changes list
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/users/user", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        User user = new User(currentNode.getAttributeValue("name"));
        List<String> groups = new ArrayList<String>();
        XPathExpression<Element> xpaGroups = XPathFactory.instance().compile(
            "./groups/g", Filters.element());
        List<Element> resultGroups = xpaGroups.evaluate(currentNode);
        Iterator<Element> itGroups = resultGroups.iterator();
        while (itGroups.hasNext()) {
          groups.add(itGroups.next().getValue());
        }
        user.setGroups(groups);
        List<String> rights = new ArrayList<String>();
        XPathExpression<Element> xpaRights = XPathFactory.instance().compile(
            "./rights/r", Filters.element());
        List<Element> resultRights = xpaRights.evaluate(currentNode);
        Iterator<Element> itRights = resultRights.iterator();
        while (itRights.hasNext()) {
          rights.add(itRights.next().getValue());
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
