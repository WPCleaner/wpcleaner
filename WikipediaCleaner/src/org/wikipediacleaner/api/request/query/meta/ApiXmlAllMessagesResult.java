/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

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
 * MediaWiki API XML messages results.
 */
public class ApiXmlAllMessagesResult extends ApiXmlResult implements ApiAllMessagesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlAllMessagesResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute message request.
   * 
   * @param properties Properties defining request.
   * @return Message.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public String executeMessage(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve general information
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/allmessages/message", Filters.element());
      Element generalNode = xpa.evaluateFirst(root);
      if (generalNode != null) {
        return generalNode.getValue();
      }

      return null;
    } catch (JDOMException e) {
      log.error("Error loading messages", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Execute messages request.
   * 
   * @param properties Properties defining request.
   * @param messages Map of messages to be filled with the results.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeMessages(
      Map<String, String> properties,
      Map<String, String> messages) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve general information
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/allmessages/message", Filters.element());
      List<Element> listMessages = xpa.evaluate(root);
      Iterator<Element> itMessages = listMessages.iterator();
      while (itMessages.hasNext()) {
        Element message = itMessages.next();
        String name = message.getAttributeValue("name");
        String text = message.getText().trim();
        messages.put(name, text);
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/allmessages",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading messages", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
