/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
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
    Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

    // Get recent changes list
    XPathExpression<Element> xpa = XPathFactory.instance().compile(
        "/api/query/users/user", Filters.element());
    List<Element> results = xpa.evaluate(root);
    for (Element currentNode : results) {
      User user = new User(currentNode.getAttributeValue("name"));
      List<String> groups = new ArrayList<>();
      XPathExpression<Element> xpaGroups = XPathFactory.instance().compile(
          "./groups/g", Filters.element());
      List<Element> resultGroups = xpaGroups.evaluate(currentNode);
      for (Element resultGroup : resultGroups) {
        groups.add(resultGroup.getValue());
      }
      user.setGroups(groups);
      List<String> rights = new ArrayList<>();
      XPathExpression<Element> xpaRights = XPathFactory.instance().compile(
          "./rights/r", Filters.element());
      List<Element> resultRights = xpaRights.evaluate(currentNode);
      for (Element resultRight : resultRights) {
        rights.add(resultRight.getValue());
      }
      user.setRights(rights);
      return user;
    }
    return null;
  }
}
