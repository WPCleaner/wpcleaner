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
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML protected titles results.
 */
public class ApiXmlProtectedTitlesResult extends ApiXmlResult implements ApiProtectedTitlesResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlProtectedTitlesResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute protected titles request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with protected titles.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeProtectedTitles(
      Map<String, String> properties,
      List<Page> list) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve embedding pages
      XPath xpa = XPath.newInstance("/api/query/protectedtitles/pt");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        if ("infinity".equals(currentNode.getAttributeValue("expiry"))) {
          Page page = DataManager.getPage(
              getWiki(), currentNode.getAttributeValue("title"), null, null, null);
          page.setNamespace(currentNode.getAttributeValue("ns"));
          list.add(page);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/protectedtitles",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading protected titles list", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
