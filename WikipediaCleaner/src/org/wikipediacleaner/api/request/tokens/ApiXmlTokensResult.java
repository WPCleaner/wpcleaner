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
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
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
   * @throws APIException
   */
  @Override
  public void executeTokens(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Get recent changes list
      XPath xpa = XPath.newInstance("/api/tokens");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        getWiki().getConnection().setDeleteToken(currentNode.getAttributeValue("deletetoken"));
        getWiki().getConnection().setEditToken(currentNode.getAttributeValue("edittoken"));
      }
    } catch (JDOMException e) {
      log.error("Error retrieving tokens", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
