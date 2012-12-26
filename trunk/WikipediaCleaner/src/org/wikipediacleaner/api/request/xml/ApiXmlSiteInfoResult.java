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
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.request.ApiSiteInfoResult;
import org.wikipediacleaner.api.request.ApiRequest;


/**
 * MediaWiki API XML site information results.
 */
public class ApiXmlSiteInfoResult extends ApiXmlResult implements ApiSiteInfoResult {

  /**
   * @param wiki Wiki on which requests are made.
   * @param httpClient HTTP client for making requests.
   */
  public ApiXmlSiteInfoResult(
      EnumWikipedia wiki,
      HttpClient httpClient) {
    super(wiki, httpClient);
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
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
      WikiConfiguration wikiConfiguration = getWiki().getWikiConfiguration();

      // Retrieve name spaces
      HashMap<Integer, Namespace> namespaces = null;
      XPath xpa = XPath.newInstance("/api/query/namespaces/ns");
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      namespaces = new HashMap<Integer, Namespace>();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String title = currentNode.getText();
        String canonical = currentNode.getAttributeValue("canonical");
        String id = currentNode.getAttributeValue("id");
        EnumCaseSensitiveness caseSensitiveness = EnumCaseSensitiveness.getCase(currentNode.getAttributeValue("case"));
        boolean subPages = (currentNode.getAttribute("subpages") != null);
        Namespace ns = new Namespace(id, title, canonical, caseSensitiveness, subPages);
        namespaces.put(ns.getId(), ns);
      }

      // Retrieve name space aliases
      xpa = XPath.newInstance("/api/query/namespacealiases/ns");
      results = xpa.selectNodes(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Integer nsId = null;
        try {
          nsId = Integer.parseInt(currentNode.getAttributeValue("id"));
          Namespace namespace = namespaces.get(nsId);
          if (namespace != null) {
            namespace.addAlias(currentNode.getText());
          }
        } catch (NumberFormatException e) {
          //
        }
      }

      // Update name space list
      LinkedList<Namespace> list = new LinkedList<Namespace>(namespaces.values());
      wikiConfiguration.setNamespaces(list);

      // Retrieve languages
      List<Language> languages = new ArrayList<Language>();
      xpa = XPath.newInstance("/api/query/languages/lang");
      results = xpa.selectNodes(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String code = currentNode.getAttributeValue("code");
        String name = currentNode.getText();
        languages.add(new Language(code, name));
      }
      wikiConfiguration.setLanguages(languages);

      // Retrieve interwikis
      List<Interwiki> interwikis = new ArrayList<Interwiki>();
      xpa = XPath.newInstance("/api/query/interwikimap/iw");
      results = xpa.selectNodes(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String prefix = currentNode.getAttributeValue("prefix");
        boolean local = (currentNode.getAttribute("local") != null);
        String language = currentNode.getAttributeValue("language");
        String url = currentNode.getAttributeValue("url");
        interwikis.add(new Interwiki( prefix, local, language, url));
      }
      wikiConfiguration.setInterwikis(interwikis);

      // Retrieve magic words
      Map<String, MagicWord> magicWords = new HashMap<String, MagicWord>();
      xpa = XPath.newInstance("/api/query/magicwords/magicword");
      results = xpa.selectNodes(root);
      iter = results.iterator();
      XPath xpaAlias = XPath.newInstance("./aliases/alias");
      XPath xpaAliasValue = XPath.newInstance(".");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String magicWord = currentNode.getAttributeValue("name");
        List<String> aliases = new ArrayList<String>();
        List resultsAlias = xpaAlias.selectNodes(currentNode);
        Iterator iterAlias = resultsAlias.iterator();
        while (iterAlias.hasNext()) {
          Element currentAlias = (Element) iterAlias.next();
          String alias = xpaAliasValue.valueOf(currentAlias);
          aliases.add(alias);
        }
        boolean caseSensitive = (currentNode.getAttribute("case-sensitive") != null);
        magicWords.put(
            magicWord,
            new MagicWord(magicWord, aliases, caseSensitive));
      }
      wikiConfiguration.setMagicWords(magicWords);
    } catch (JDOMException e) {
      log.error("Error loading namespaces", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
