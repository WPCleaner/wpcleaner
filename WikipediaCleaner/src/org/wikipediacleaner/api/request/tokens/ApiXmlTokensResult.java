/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.tokens;

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
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML tokens results.
 */
public class ApiXmlTokensResult extends ApiXmlResult implements ApiTokensResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlTokensResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute tokens request.
   * 
   * @param properties Properties defining request.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void executeTokens(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Get recent changes list
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/tokens", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String deleteToken = currentNode.getAttributeValue("deletetoken");
        if (deleteToken != null) {
          getWiki().getConnection().setDeleteToken(deleteToken);
        }
        String editToken = currentNode.getAttributeValue("edittoken");
        if (editToken != null) {
          getWiki().getConnection().setEditToken(editToken);
        }
      }
    } catch (JDOMException e) {
      log.error("Error retrieving tokens", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
