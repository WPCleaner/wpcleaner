/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.parse;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.xpath.XPath;
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
   * @throws APIException
   */
  @Override
  public String executeParse(
      Map<String, String> properties)
          throws APIException {
    try {
      XPath xpaContents = XPath.newInstance("/api/parse/text/.");
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
      return xpaContents.valueOf(root);
    } catch (JDOMException e) {
      log.error("Error expanding templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Execute sections request.
   * 
   * @param page Page.
   * @param properties Properties defining request.
   * @return List of sections.
   * @throws APIException
   */
  @Override
  public List<Section> executeSections(
      Page page, Map<String, String> properties)
          throws APIException {
    try {
      Element root = null;
      try {
        root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
      } catch (APIException e) {
        if (EnumQueryResult.MISSING_TITLE.equals(EnumQueryResult.MISSING_TITLE)) {
          return null;
        }
        throw e;
      }

      // Retrieve sections
      XPath xpaSections = XPath.newInstance("/api/parse/sections/s");
      List listSections = xpaSections.selectNodes(root);
      List<Section> result = new ArrayList<Section>(listSections.size());
      Iterator itSection = listSections.iterator();
      while (itSection.hasNext()) {
        Element sectionNode = (Element) itSection.next();
        try {
          Section section = new Section(
              Integer.valueOf(sectionNode.getAttributeValue("toclevel")),
              Integer.valueOf(sectionNode.getAttributeValue("level")),
              sectionNode.getAttributeValue("line"),
              sectionNode.getAttributeValue("number"),
              Integer.valueOf(sectionNode.getAttributeValue("index")));
          result.add(section);
        } catch (NumberFormatException e) {
          //
        }
      }

      // Retrieve revision id
      XPath xpaPage = XPath.newInstance("/api/parse");
      Element parseNode = (Element) xpaPage.selectSingleNode(root);
      if ((parseNode != null) && (parseNode.getAttributeValue("revid") != null)) {
        page.setRevisionId(parseNode.getAttributeValue("revid"));
      }

      return result;
    } catch (JDOMException e) {
      log.error("Error retrieving sections", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
