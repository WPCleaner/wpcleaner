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
 * Class for MediaWiki API XML raw watch list results.
 */
public class ApiXmlRawWatchlistResult extends ApiXmlResult implements ApiRawWatchlistResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlRawWatchlistResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute watch list raw request.
   * 
   * @param properties Properties defining request.
   * @param watchlist List of pages to be filled with the watch list.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeWatchlistRaw(
      Map<String, String> properties,
      List<Page> watchlist)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve watch list
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/watchlistraw/wr", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        Page page = DataManager.getPage(
            getWiki(), currentNode.getAttributeValue("title"), null, null, null);
        if (page.isArticle()) {
          watchlist.add(page);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/watchlistraw",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading watch list", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
