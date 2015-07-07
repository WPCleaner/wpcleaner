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
import org.wikipediacleaner.api.request.ApiLanguageLinksResult;
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
      XPath xpa = XPath.newInstance("/api/query/pages/page/langlinks/ll");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaTitle = XPath.newInstance(".");
      XPath xpaLang = XPath.newInstance("./@lang");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String title = xpaTitle.valueOf(currentNode);
        String lang = xpaLang.valueOf(currentNode);
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
