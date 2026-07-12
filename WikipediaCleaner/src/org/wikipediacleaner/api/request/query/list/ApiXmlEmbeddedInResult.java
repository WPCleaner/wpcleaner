/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.list;

import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
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
 * MediaWiki API XML embedded in results.
 */
public class ApiXmlEmbeddedInResult extends ApiXmlResult implements ApiEmbeddedInResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlEmbeddedInResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute embedded in request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with embedding pages.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeEmbeddedIn(
      Map<String, String> properties,
      List<Page> list) throws APIException {
    Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

    // Retrieve embedding pages
    XPathExpression<Element> xpa = XPathFactory.instance().compile(
        "/api/query/embeddedin/ei", Filters.element());
    List<Element> results = xpa.evaluate(root);
    for (Element currentNode : results) {
      Page page = DataManager.getPage(
          getWiki(), currentNode.getAttributeValue("title"), null, null, null);
      page.setNamespace(currentNode.getAttributeValue("ns"));
      page.setPageId(currentNode.getAttributeValue("pageid"));
      list.add(page);
    }

    // Retrieve continue
    return shouldContinue(
        root, "/api/query-continue/embeddedin",
        properties);
  }
}
