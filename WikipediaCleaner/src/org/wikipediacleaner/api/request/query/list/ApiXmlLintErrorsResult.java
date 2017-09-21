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
 * MediaWiki API XML category members results.
 */
public class ApiXmlLintErrorsResult extends ApiXmlResult implements ApiLintErrorsResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlLintErrorsResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute lint errors request.
   * 
   * @param properties Properties defining request.
   * @param list List to be filled with lint errors.
   * @param category Linter category.
   * @param withTemplates Includes templates causing the error.
   * @return True if request should be continued.
   * @throws APIException
   */
  @Override
  public boolean executeLinterCategory(
      Map<String, String> properties,
      List<Page> list,
      String category, boolean withTemplates) throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);

      // Retrieve lint errors
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/linterrors/_v", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        Page page = DataManager.getPage(
            getWiki(), currentNode.getAttributeValue("title"), null, null, null);
        if (!list.contains(page)) {
          list.add(page);
        }
        if (withTemplates) {
          Element templateInfo = currentNode.getChild("templateInfo");
          if (templateInfo != null) {
            String templateName = templateInfo.getAttributeValue("name");
            if ((templateName != null) &&
                !templateName.startsWith("{") &&
                !templateName.startsWith("#")) {
              page = DataManager.getPage(
                  getWiki(), templateName, null, null, null);
              if (!list.contains(page)) {
                list.add(page);
              }
            }
          }
        }
      }

      // Retrieve continue
      return shouldContinue(
          root, "/api/continue",
          properties);
    } catch (JDOMException e) {
      log.error("Error loading category members list", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
