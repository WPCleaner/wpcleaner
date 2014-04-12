/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.xml;

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
import org.wikipediacleaner.api.request.ApiPagesWithPropResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML pages with property results.
 */
public class ApiXmlPagesWithPropResult extends ApiXmlResult implements ApiPagesWithPropResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlPagesWithPropResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute pages with property request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with protected titles.
   * @return True if request should be continued.
   * @throws APIException
   */
  public boolean executePagesWithProp(
      Map<String, String> properties,
      List<Page> list) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve embedding pages
      XPath xpa = XPath.newInstance("/api/query/pageswithprop/page");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Integer pageId = null;
        try {
          String tmp = currentNode.getAttributeValue("pageid");
          if (tmp != null) {
            pageId = Integer.valueOf(tmp);
          }
        } catch (NumberFormatException e) {
          //
        }
        Page page = DataManager.getPage(
            getWiki(), currentNode.getAttributeValue("title"),
            pageId, null, null);
        page.setNamespace(currentNode.getAttributeValue("ns"));
        list.add(page);
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/pageswithprop",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading protected titles list", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}