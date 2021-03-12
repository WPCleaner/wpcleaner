/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.request.query.meta;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.filter.Filters;
import org.jdom2.xpath.XPathExpression;
import org.jdom2.xpath.XPathFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.configuration.WikiConfiguration;
import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.SpecialPage;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiXmlResult;


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
   * @throws APIException Exception thrown by the API.
   */
  @Override
  public void executeSiteInformation(
      Map<String, String> properties)
          throws APIException {
    try {
      Element root = getRoot(properties, ApiRequest.MAX_ATTEMPTS);
      WikiConfiguration wikiConfiguration = getWiki().getWikiConfiguration();

      // Retrieve general information
      XPathExpression<Element> xpa = XPathFactory.instance().compile(
          "/api/query/general", Filters.element());
      Element generalNode = xpa.evaluateFirst(root);
      if (generalNode != null) {
        wikiConfiguration.setArticlePath(generalNode.getAttributeValue("articlepath"));
        wikiConfiguration.setMaxArticleSize(generalNode.getAttributeValue("maxarticlesize"));
        wikiConfiguration.setScript(generalNode.getAttributeValue("script"));
        wikiConfiguration.setServer(generalNode.getAttributeValue("server"));
      }

      // Retrieve name spaces
      HashMap<Integer, Namespace> namespaces = null;
      xpa = XPathFactory.instance().compile(
          "/api/query/namespaces/ns", Filters.element());
      List<Element> results = xpa.evaluate(root);
      Iterator<Element> iter = results.iterator();
      namespaces = new HashMap<>();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String title = currentNode.getText();
        String canonical = currentNode.getAttributeValue("canonical");
        String id = currentNode.getAttributeValue("id");
        EnumCaseSensitiveness caseSensitiveness = EnumCaseSensitiveness.getCase(currentNode.getAttributeValue("case"));
        boolean subPages = (currentNode.getAttribute("subpages") != null);
        Namespace ns = new Namespace(id, title, canonical, caseSensitiveness, subPages);
        namespaces.put(ns.getId(), ns);
      }

      // Retrieve name space aliases
      xpa = XPathFactory.instance().compile(
          "/api/query/namespacealiases/ns", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
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
      LinkedList<Namespace> list = new LinkedList<>(namespaces.values());
      wikiConfiguration.setNamespaces(list);

      // Retrieve languages
      List<Language> languages = new ArrayList<>();
      xpa = XPathFactory.instance().compile(
          "/api/query/languages/lang", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String code = currentNode.getAttributeValue("code");
        String name = currentNode.getText();
        languages.add(new Language(code, name));
      }
      wikiConfiguration.setLanguages(languages);

      // Retrieve interwikis
      List<Interwiki> interwikis = new ArrayList<>();
      xpa = XPathFactory.instance().compile(
          "/api/query/interwikimap/iw", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String prefix = currentNode.getAttributeValue("prefix");
        boolean local = (currentNode.getAttribute("local") != null);
        String language = currentNode.getAttributeValue("language");
        String url = currentNode.getAttributeValue("url");
        interwikis.add(new Interwiki(prefix, local, language, url));
      }
      wikiConfiguration.setInterwikis(interwikis);

      // Retrieve magic words
      List<MagicWord> magicWords = new ArrayList<>();
      xpa = XPathFactory.instance().compile(
          "/api/query/magicwords/magicword", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      XPathExpression<Element> xpaAlias = XPathFactory.instance().compile(
          "./aliases/alias", Filters.element());
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String magicWord = currentNode.getAttributeValue("name");
        List<String> aliases = new ArrayList<>();
        List<Element> resultsAlias = xpaAlias.evaluate(currentNode);
        Iterator<Element> iterAlias = resultsAlias.iterator();
        while (iterAlias.hasNext()) {
          Element currentAlias = iterAlias.next();
          String alias = currentAlias.getText();
          aliases.add(alias);
        }
        boolean caseSensitive = (currentNode.getAttribute("case-sensitive") != null);
        magicWords.add(new MagicWord(magicWord, aliases, caseSensitive));
      }
      wikiConfiguration.setMagicWords(magicWords);

      // Retrieve special page aliases
      Map<String, SpecialPage> specialPages = new HashMap<>();
      xpa = XPathFactory.instance().compile(
          "/api/query/specialpagealiases/specialpage", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String specialPage = currentNode.getAttributeValue("realname");
        List<String> aliases = new ArrayList<>();
        List<Element> resultsAlias = xpaAlias.evaluate(currentNode);
        Iterator<Element> iterAlias = resultsAlias.iterator();
        while (iterAlias.hasNext()) {
          Element currentAlias = iterAlias.next();
          String alias = currentAlias.getText();
          aliases.add(alias);
        }
        specialPages.put(
            specialPage,
            new SpecialPage(specialPage, aliases));
      }
      wikiConfiguration.setSpecialPages(specialPages);

      // Retrieve linter configuration
      List<LinterCategory> linterCategories = new ArrayList<>();
      xpa = XPathFactory.instance().compile(
          "/api/query/general/linter/*", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String level = currentNode.getName();
        for (Element child : currentNode.getChildren()) {
          linterCategories.add(new LinterCategory(level, child.getTextTrim()));
        }
      }
      wikiConfiguration.setLinterCategories(linterCategories);

      // Retrieve extensions
      xpa = XPathFactory.instance().compile(
          "/api/query/extensions/ext", Filters.element());
      results = xpa.evaluate(root);
      iter = results.iterator();
      while (iter.hasNext()) {
        Element currentNode = iter.next();
        String name = currentNode.getAttributeValue("name");
        if ((name != null) && (name.equals("Translate"))) {
          wikiConfiguration.setTranslatable(true);
        }
      }
    } catch (JDOMException e) {
      log.error("Error loading namespaces", e);
      throw new APIException("Error parsing XML", e);
    }
  }
}
