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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML category members results.
 */
public class ApiXmlCategoryMembersResult extends ApiXmlResult implements ApiCategoryMembersResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlCategoryMembersResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute category members request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with category members.
   * @param categories Map of categories to be analyzed with their depth.
   * @param depth Current depth of the analysis.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeCategoryMembers(
      Map<String, String> properties,
      List<Page> list,
      Map<Page, Integer> categories, int depth) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve category members
      XPath xpa = XPath.newInstance("/api/query/categorymembers/cm");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaPageId = XPath.newInstance("./@pageid");
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Page page = DataManager.getPage(
            getWiki(), xpaTitle.valueOf(currentNode), null, null, null);
        page.setNamespace(xpaNs.valueOf(currentNode));
        page.setPageId(xpaPageId.valueOf(currentNode));
        if ((page.getNamespace() != null) &&
            (page.getNamespace().intValue() == Namespace.CATEGORY)) {
          categories.put(page, depth + 1);
        } else {
          if (!list.contains(page)) {
            list.add(page);
          }
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/query-continue/categorymembers",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading category members list", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
