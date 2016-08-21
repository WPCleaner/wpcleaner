/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

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
  @Override
  public boolean getLanguageLinks(
      Map<String, String> properties,
      Map<String, String> languageLinks) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Set disambiguation status
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/pages/page/langlinks/ll", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String title = currentNode.getText();
        String lang = currentNode.getAttributeValue("lang");
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
