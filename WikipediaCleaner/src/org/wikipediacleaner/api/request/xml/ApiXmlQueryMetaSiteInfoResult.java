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
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.JDOMParseException;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.request.ApiQueryMetaSiteInfoResult;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ConnectionInformation;


/**
 * Class for MediaWiki API XML site information results.
 */
public class ApiXmlQueryMetaSiteInfoResult extends ApiXmlResult implements ApiQueryMetaSiteInfoResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   * @param connection Connection information.
   */
  public ApiXmlQueryMetaSiteInfoResult(
      EnumWikipedia wiki,
      HttpClient httpClient,
      ConnectionInformation connection) {
    super(wiki, httpClient, connection);
  }

  /**
   * Execute site information request.
   * 
   * @param properties Properties defining request.
   * @throws APIException
   */
  public void executeSiteInformation(
      Map<String, String> properties)
          throws APIException {
    try {
      constructSiteInfo(
          getRoot(properties, ApiRequest.MAX_ATTEMPTS),
          "/api/query");
    } catch (JDOMParseException e) {
      log.error("Error loading namespaces", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * @param root Root element.
   * @param query XPath query to retrieve the name spaces
   * @throws APIException
   */
  private void constructSiteInfo(
      Element root, String query)
      throws APIException {

    // Retrieve name spaces
    HashMap<Integer, Namespace> namespaces = null;
    try {
      XPath xpa = XPath.newInstance(query + "/namespaces/ns");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      namespaces = new HashMap<Integer, Namespace>();
      XPath xpaId = XPath.newInstance("./@id");
      XPath xpaTitle = XPath.newInstance(".");
      XPath xpaCanonical = XPath.newInstance("./@canonical");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Namespace ns = new Namespace(
            xpaId.valueOf(currentNode),
            xpaTitle.valueOf(currentNode),
            xpaCanonical.valueOf(currentNode));
        namespaces.put(ns.getId(), ns);
      }
    } catch (JDOMException e) {
      log.error("Error namespaces", e);
      throw new APIException("Error parsing XML result", e);
    }

    // Retrieve name space aliases
    try {
      XPath xpa = XPath.newInstance(query + "/namespacealiases/ns");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaId = XPath.newInstance("./@id");
      XPath xpaTitle = XPath.newInstance(".");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Integer nsId = null;
        try {
          nsId = Integer.parseInt(xpaId.valueOf(currentNode));
          Namespace namespace = namespaces.get(nsId);
          if (namespace != null) {
            namespace.addAlias(xpaTitle.valueOf(currentNode));
          }
        } catch (NumberFormatException e) {
          //
        }
      }
    } catch (JDOMException e) {
      log.error("Error namespaces", e);
      throw new APIException("Error parsing XML result", e);
    }

    // Update name space list
    LinkedList<Namespace> list = new LinkedList<Namespace>(namespaces.values());
    getWiki().setNamespaces(list);

    // Retrieve languages
    try {
      List<Language> languages = new ArrayList<Language>();
      XPath xpa = XPath.newInstance(query + "/languages/lang");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaCode = XPath.newInstance("./@code");
      XPath xpaName = XPath.newInstance(".");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        languages.add(new Language(xpaCode.valueOf(currentNode), xpaName.valueOf(currentNode)));
      }
      getWiki().setLanguages(languages);
    } catch (JDOMException e) {
      log.error("Error languages", e);
      throw new APIException("Error parsing XML result", e);
    }

    // Retrieve interwikis
    try {
      List<Interwiki> interwikis = new ArrayList<Interwiki>();
      XPath xpa = XPath.newInstance(query + "/interwikimap/iw");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaPrefix = XPath.newInstance("./@prefix");
      XPath xpaLocal = XPath.newInstance("./@local");
      XPath xpaLanguage = XPath.newInstance("./@language");
      XPath xpaUrl = XPath.newInstance("./@url");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        interwikis.add(new Interwiki(
            xpaPrefix.valueOf(currentNode),
            xpaLocal.valueOf(currentNode),
            xpaLanguage.valueOf(currentNode),
            xpaUrl.valueOf(currentNode)));
      }
      getWiki().setInterwikis(interwikis);
    } catch (JDOMException e) {
      log.error("Error languages", e);
      throw new APIException("Error parsing XML result", e);
    }

    // Retrieve magic words
    try {
      Map<String, MagicWord> magicWords = new HashMap<String, MagicWord>();
      XPath xpa = XPath.newInstance(query + "/magicwords/magicword");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaName = XPath.newInstance("./@name");
      XPath xpaAlias = XPath.newInstance("./aliases/alias");
      XPath xpaAliasValue = XPath.newInstance(".");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String magicWord = xpaName.valueOf(currentNode);
        List<String> aliases = new ArrayList<String>();
        List resultsAlias = xpaAlias.selectNodes(currentNode);
        Iterator iterAlias = resultsAlias.iterator();
        while (iterAlias.hasNext()) {
          Element currentAlias = (Element) iterAlias.next();
          String alias = xpaAliasValue.valueOf(currentAlias);
          aliases.add(alias);
        }
        magicWords.put(magicWord, new MagicWord(magicWord, aliases));
      }
      getWiki().setMagicWords(magicWords);
    } catch (JDOMException e) {
      log.error("Error magic words", e);
      throw new APIException("Error parsing XML result", e);
    }
  }
}
