/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.prop;

import java.util.ArrayList;
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


/**
 * MediaWiki API XML links results.
 */
public class ApiXmlLinksResult extends ApiXmlPropertiesResult implements ApiLinksResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLinksResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute links request.
   * 
   * @param properties Properties defining request.
   * @param lists Map of lists to be filled with links.
   * @param normalization Map containing information about title normalization (From => To).
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeLinks(
      Map<String, String> properties,
      Map<String, List<Page>> lists,
      Map<String, String> normalization) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve normalization information
      retrieveNormalization(root, normalization);

      // Retrieve back links
      XPath xpaPages = XPath.newInstance("/api/query/pages/page");
      List listPages = xpaPages.selectNodes(root);
      Iterator itPage = listPages.iterator();
      XPath xpaLinks = XPath.newInstance("links/pl");
      while (itPage.hasNext()) {
        Element pageNode = (Element) itPage.next();
        String pageTitle = pageNode.getAttributeValue("title");
        List<Page> links = lists.get(pageTitle);
        if (links == null) {
          links = new ArrayList<Page>();
          lists.put(pageTitle, links);
        }
        List listLinks = xpaLinks.selectNodes(pageNode);
        Iterator itLinks = listLinks.iterator();
        while (itLinks.hasNext()) {
          Element linkNode = (Element) itLinks.next();
          Page link = DataManager.getPage(
              getWiki(), linkNode.getAttributeValue("title"), null, null, null);
          link.setNamespace(linkNode.getAttributeValue("ns"));
          links.add(link);
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/links",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading links", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Execute links request.
   * 
   * @param properties Properties defining request.
   * @param links List to be filled with links.
   * @param knownPages Already known pages.
   * @param normalization Map containing information about title normalization (From => To).
   * @param redirects List of redirects filled by the method.
   * @param useDisambig Flag indicating if disambiguation property should be used.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeLinks(
      Map<String, String> properties,
      List<Page> links,
      List<Page> knownPages,
      Map<String, String> normalization,
      List<Page> redirects, boolean useDisambig) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve normalization information
      retrieveNormalization(root, normalization);

      // Retrieve back links
      XPath xpaPages = XPath.newInstance("/api/query/pages/page");
      List listLinks = xpaPages.selectNodes(root);
      Iterator itLinks = listLinks.iterator();
      while (itLinks.hasNext()) {
        Element linkNode = (Element) itLinks.next();
        Page link = getPage(getWiki(), linkNode, knownPages, useDisambig);
        if ((redirects != null) && (link.isRedirect())) {
          redirects.add(link);
        }
        links.add(link);
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/links",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading links", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
