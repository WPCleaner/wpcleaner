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
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML redirects results.
 */
public class ApiXmlRedirectsResult extends ApiXmlResult implements ApiRedirectsResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlRedirectsResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute redirects request.
   * 
   * @param properties Properties defining request.
   * @param page Page.
   * @param list List to be filled with redirects to the page.
   * @return True if request should be continued.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public boolean executeRedirects(
      Map<String, String> properties,
      Page page,
      List<Page> list)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve redirects
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/pages/page/redirects/rd", Filters.element());
      List<Element> listRedirects = xpa.evaluate(root);
      Iterator<Element> itRedirects = listRedirects.iterator();
      while (itRedirects.hasNext()) {
        Element currentRedirect = itRedirects.next();
        Page link = DataManager.getPage(
            getWiki(), currentRedirect.getAttributeValue("title"), null, null, null);
        link.setNamespace(currentRedirect.getAttributeValue("ns"));
        link.setPageId(currentRedirect.getAttributeValue("pageid"));
        link.getRedirects().add(page, null); // TODO: Check if fragment is available
        if (!list.contains(link)) {
          list.add(link);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/redirects",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading redirects", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
