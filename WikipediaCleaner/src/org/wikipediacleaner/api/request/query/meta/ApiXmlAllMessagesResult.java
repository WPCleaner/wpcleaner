/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
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
   * @throws APIException
   */
  @Override
  public String executeMessage(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve general information
      XPath xpa = XPath.newInstance("/api/query/allmessages/message");
      Element generalNode = (Element) xpa.selectSingleNode(root);
      if (generalNode != null) {
        return generalNode.getValue();
      }

      return null;
    } catch (JDOMException e) {
      log.error("Error loading messages", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
