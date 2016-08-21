/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

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
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * Class for MediaWiki API XML search results.
 */
public class ApiXmlSearchResult extends ApiXmlResult implements ApiSearchResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlSearchResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute search request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with back links.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeSearch(
      Map<String, String> properties,
      List<Page> list)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve search results
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/search/p", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        Page similarPage = DataManager.getPage(
            getWiki(), currentNode.getAttributeValue("title"), null, null, null);
        similarPage.setNamespace(currentNode.getAttributeValue("ns"));
        list.add(similarPage);
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/search",
          properties);
    } catch (JDOMException e) {
      log.error("Error searching", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
