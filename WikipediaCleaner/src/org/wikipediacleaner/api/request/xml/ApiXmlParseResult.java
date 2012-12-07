/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.request.xml;

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
import org.wikipediacleaner.api.request.ApiParseResult;
import org.wikipediacleaner.api.request.ApiRequest;


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
