/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.parse;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
import org.jdom2.filter.Filters;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Section;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


/**
 * MediaWiki API XML parse results.
 */
public class ApiXmlParseResult extends ApiXmlResult implements ApiParseResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlParseResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
  }

  /**
   * Execute parse request.
   * 
   * @param properties Properties defining request.
   * @return Parsed text.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public String executeParse(
      Map<String, String> properties)
          throws APIException {
    XPathExpression<Element> xpaText = XPathFactory.instance().compile(
        "/api/parse/text", Filters.element());
    Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
    Element text = xpaText.evaluateFirst(root);
    return (text != null) ? text.getText() : null;
  }

  /**
   * Execute sections request.
   * 
   * @param page Page.
   * @param properties Properties defining request.
   * @return List of sections.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public List<Section> executeSections(
      Page page, Map<String, String> properties)
          throws APIException {
    Element root = null;
    try {
      root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
    } catch (APIException e) {
      if (EnumQueryResult.MISSING_TITLE.equals(e.getQueryResult())) {
        return null;
      }
      throw e;
    }

    // Retrieve sections
    XPathExpression<Element> xpaSections = XPathFactory.instance().compile(
        "/api/parse/sections/s", Filters.element());
    List<Element> listSections = xpaSections.evaluate(root);
    List<Section> result = new ArrayList<>(listSections.size());
    for (Element sectionNode : listSections) {
      try {
        Section section = new Section(
            Integer.parseInt(sectionNode.getAttributeValue("toclevel")),
            Integer.parseInt(sectionNode.getAttributeValue("level")),
            sectionNode.getAttributeValue("line"),
            sectionNode.getAttributeValue("number"),
            Integer.parseInt(sectionNode.getAttributeValue("index")));
        result.add(section);
      } catch (NumberFormatException e) {
        //
      }
    }

    // Retrieve revision id
    XPathExpression<Element> xpaPage = XPathFactory.instance().compile(
        "/api/parse", Filters.element());
    Element parseNode = xpaPage.evaluateFirst(root);
    if ((parseNode != null) && (parseNode.getAttributeValue("revid") != null)) {
      page.setRevisionId(parseNode.getAttributeValue("revid"));
    }

    return result;
  }
}
