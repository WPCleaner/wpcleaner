/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.api.impl;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.JDOMParseException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.jdom.xpath.XPath;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.QueryResult;


/**
 * MediaWiki API implementation.
 */
public class MediaWikiAPI implements API {

  private final Log log = LogFactory.getLog(MediaWikiAPI.class);

  private final static String ACTION_API_EDIT   = "edit";
  private final static String ACTION_API_EXPAND = "expandtemplates";
  private final static String ACTION_API_LOGIN  = "login";
  private final static String ACTION_API_PARSE  = "parse";
  private final static String ACTION_API_PURGE  = "purge";
  private final static String ACTION_API_QUERY  = "query";

  private final static int MAX_PAGES_PER_QUERY = 50;
  private final static int MAX_ATTEMPTS = 2;

  private final static boolean DEBUG_TIME = false;
  private final static boolean DEBUG_URL = true;
  private final static boolean DEBUG_XML = false;
  private static XMLOutputter xmlOutputter;

  private EnumWikipedia mediawikiAPI;
  private HttpClient httpClient;

  private String username = null;
  private String password = null;
  private String lgtoken = null;
  private String lgusername = null;
  private String lguserid = null;

  private HashMap<Integer, Namespace> namespaces;

  /**
   * Constructor.
   */
  public MediaWikiAPI() {
    MultiThreadedHttpConnectionManager manager = new MultiThreadedHttpConnectionManager();
    httpClient = new HttpClient(manager);
  }

  /**
   * @return Maximum number of pages per query.
   */
  public int getMaxPagesPerQuery() {
    return MAX_PAGES_PER_QUERY;
  }

  /**
   * Login into Wikipedia.
   * 
   * @param wikipedia Wikipedia URL.
   * @param username1 User name.
   * @param password1 Password.
   * @return Login status.
   */
  public LoginResult login(
      EnumWikipedia wikipedia,
      String username1,
      String password1) throws APIException {

    // Login
    this.username = username1;
    this.password = password1;
    logout();
    this.mediawikiAPI = wikipedia;
    lgtoken = null;
    lgusername = null;
    lguserid = null;
    HashMap<String, String> properties = getProperties(ACTION_API_LOGIN, true);
    properties.put("lgname", username);
    properties.put("lgpassword", password);
    LoginResult result = null;
    try {
      result = constructLogin(
          getRoot(properties, 1),
          "/api/login");
    } catch (JDOMParseException e) {
      log.error("Exception in MediaWikiAPI.login()", e);
      throw new APIException("Couldn't login");
    }

    // Retrieve data
    loadNamespaces();

    return result;
  }

  /**
   * Logout.
   */
  public void logout() {
    this.mediawikiAPI = null;
    this.lgtoken = null;
    this.lgusername = null;
    this.lguserid = null;
    this.namespaces = null;
  }

  /**
   * @param id Namespace Id.
   * @return Name of the namespace.
   */
  public String getNamespace(Integer id) {
    Namespace ns = namespaces.get(id);
    return (ns != null ? ns.getTitle() : "");
  }

  /**
   * Retrieves the list of Namespaces.
   * 
   * @param list List of Namespaces. 
   */
  public void getNamespaces(List<Namespace> list) {
    if ((list != null) && (namespaces != null)) {
      for (Namespace n : namespaces.values()) {
        list.add(n);
      }
      Collections.sort(list);
    }
  }

  /**
   * @return The title of a random page.
   * @throws APIException
   */
  //TODO: Use api.php: action=query&list=random
  public String getRandomPage() throws APIException {
    BufferedReader in = null;
    try {
      String url = mediawikiAPI.getWikiURL() + "?title=Special:Random";
      URLConnection connection = new URL(url).openConnection();
      in = new BufferedReader(new InputStreamReader(connection.getInputStream(), "UTF-8"));
      String line = null;
      while ((line = in.readLine()) != null) {
        if (line.indexOf("wgTitle") != -1) {
          int x = line.indexOf("\"") + 1;
          return line.substring(x, line.indexOf('\"', x)).replaceAll("\\\\'", "'");
        }
      }
    } catch (IOException e) {
      log.error("Couldn't get random page", e);
      throw new APIException("Error getting random page");
    } finally {
      if (in != null) {
        try {
          in.close();
        } catch (IOException e) {
          log.error("Error closing Reader", e);
        }
      }
    }
    return null;
  }

  /**
   * Retrieves the contents of <code>page</code>.
   * 
   * @param page The page.
   */
  public void retrieveContents(Page page)
      throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("prop", "revisions|info");
    properties.put("titles", page.getTitle());
    properties.put("rvprop", "content|ids|timestamp");
    properties.put("intoken", "edit");
    try {
      boolean redirect = constructContents(
          page,
          getRoot(properties, MAX_ATTEMPTS),
          "/api/query/pages/page");
      if (redirect) {
        ArrayList<Page> pages = new ArrayList<Page>(1);
        pages.add(page);
        initializeRedirect(pages);
      }
    } catch (JDOMParseException e) {
      log.error("Error retrieving page content", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Expand templates in a text.
   * 
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Text with templates expanded.
   * @throws APIException
   */
  public String expandTemplates(String title, String text) throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_EXPAND, true);
    properties.put("title", title);
    properties.put("text", text);
    try {
      XPath xpaContents = XPath.newInstance("/api/expandtemplates/.");
      Element root = getRoot(properties, MAX_ATTEMPTS);
      return xpaContents.valueOf(root);
    } catch (JDOMException e) {
      log.error("Error expanding templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Parse text.
   * 
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Parsed text.
   * @throws APIException
   */
  public String parseText(String title, String text) throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_PARSE, true);
    properties.put("title", title);
    properties.put("text", text);
    try {
      XPath xpaContents = XPath.newInstance("/api/parse/text/.");
      Element root = getRoot(properties, MAX_ATTEMPTS);
      return xpaContents.valueOf(root);
    } catch (JDOMException e) {
      log.error("Error expanding templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Update a page on Wikipedia.
   * 
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updatePage(Page page, String newContents, String comment) throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    if (newContents == null) {
      throw new APIException("Contents is null");
    }
    if (comment == null) {
      throw new APIException("Comment is null");
    }
    QueryResult result = null;
    HashMap<String, String> properties = getProperties(ACTION_API_EDIT, true);
    properties.put("basetimestamp", page.getContentsTimestamp());
    properties.put("bot", "");
    properties.put("minor", "");
    properties.put("summary", comment);
    properties.put("text", newContents);
    properties.put("title", page.getTitle());
    properties.put("token", page.getEditToken());
    try {
      result = constructEdit(
          getRoot(properties, 1),
          "/api/edit");
    } catch (JDOMParseException e) {
      log.error("Error updating page: " + e.getMessage());
      throw new APIException("Error parsing XML", e);
    }
    return result;
  }

  /**
   * Add a new section in a page.
   * 
   * @param page Page.
   * @param title Title of the new section.
   * @param contents Contents.
   * @param editToken Edit token.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult addNewSection(String page, String title, String contents, String editToken, boolean forceWatch) throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    if (title == null) {
      throw new APIException("Title is null");
    }
    if (contents == null) {
      throw new APIException("Contents is null");
    }
    QueryResult result = null;
    HashMap<String, String> properties = getProperties(ACTION_API_EDIT, true);
    properties.put("minor", "");
    properties.put("section", "new");
    properties.put("summary", title);
    properties.put("title", page);
    properties.put("text", contents);
    properties.put("token", editToken);
    if (forceWatch) {
      properties.put("watch", "");
    }
    try {
      result = constructEdit(
          getRoot(properties, 1),
          "/api/edit");
    } catch (JDOMParseException e) {
      log.error("Error updating page: " + e.getMessage());
      throw new APIException("Error parsing XML", e);
    }
    return result;
  }

  /**
   * Purge the cache of <code>page</code>.
   * 
   * @param page The page.
   */
  public void purgePageCache(Page page)
      throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_PURGE, true);
    properties.put("titles", page.getTitle());
    try {
      checkForError(getRoot(properties, MAX_ATTEMPTS));
    } catch (JDOMParseException e) {
      log.error("Error purging page cache", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Retrieves the links of <code>page</code>.
   * 
   * @param page The page.
   */
  public void retrieveLinks(Page page)
      throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("pllimit", "max");
    properties.put("prop", "links");
    properties.put("titles", page.getTitle());
    try {
      constructLinks(
          page,
          getRoot(properties, MAX_ATTEMPTS),
          "/api/query/pages/page/links/pl");
    } catch (JDOMParseException e) {
      log.error("Error retrieving page content", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Retrieves the links of <code>page</code>.
   * 
   * @param page The page.
   */
  public void retrieveLinksWithRedirects(Page page)
      throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("generator", "links");
    properties.put("gpllimit", "max");
    properties.put("prop", "info");
    properties.put("titles", page.getTitle());
    boolean keepLinks = false;
    boolean gplcontinue = false;
    ArrayList<Page> redirects = new ArrayList<Page>();
    do {
      try {
        Element root = getRoot(properties, MAX_ATTEMPTS);
        constructLinksWithRedirects(
            page, root,
            "/api/query/pages/page",
            redirects, keepLinks);
        XPath xpaContinue = XPath.newInstance("/api/query-continue/links");
        XPath xpaGplContinue = XPath.newInstance("./@gplcontinue");
        List results = xpaContinue.selectNodes(root);
        Iterator iter = results.iterator();
        keepLinks = true;
        gplcontinue = false;
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          gplcontinue = true;
          properties.put("gplcontinue", xpaGplContinue.valueOf(currentNode));
        }
      } catch (JDOMException e) {
        log.error("Error retrieving page content", e);
        throw new APIException("Error parsing XML", e);
      }
    } while (gplcontinue);
    if (!redirects.isEmpty()) {
      initializeRedirect(redirects);
    }
  }

  /**
   * Retrieves the back links of <code>page</code>.
   * 
   * @param page The page.
   */
  public void retrieveBackLinks(Page page)
      throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("list", "backlinks");
    properties.put("bltitle", page.getTitle());
    properties.put("bllimit", "max" /*"500"*/);
    ArrayList<Page> links = new ArrayList<Page>();
    boolean blcontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/backlinks/bl");
        Element root = getRoot(properties, MAX_ATTEMPTS);
        List results = xpa.selectNodes(root);
        Iterator iter = results.iterator();
        links.ensureCapacity(links.size() + results.size());
        XPath xpaPageId = XPath.newInstance("./@pageid");
        XPath xpaNs = XPath.newInstance("./@ns");
        XPath xpaTitle = XPath.newInstance("./@title");
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          Page link = DataManager.getPage(
              page.getWikipedia(), xpaTitle.valueOf(currentNode), null);
          link.setNamespace(xpaNs.valueOf(currentNode));
          link.setPageId(xpaPageId.valueOf(currentNode));
          links.add(link);
        }
        XPath xpaContinue = XPath.newInstance("/api/query-continue/backlinks");
        XPath xpaBlContinue = XPath.newInstance("./@blcontinue");
        results = xpaContinue.selectNodes(root);
        iter = results.iterator();
        blcontinue = false;
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          properties.remove("bltitle");
          blcontinue = true;
          properties.put("blcontinue", xpaBlContinue.valueOf(currentNode));
        }
      } catch (JDOMException e) {
        log.error("Error backlinks for page " + page.getTitle(), e);
        throw new APIException("Error parsing XML result", e);
      }
    } while (blcontinue);
    Collections.sort(links);
    page.setBackLinks(links);
  }

  /**
   * Retrieves the back links of <code>page</code> and initialize redirect status.
   * 
   * @param page The page.
   */
  public void retrieveBackLinksWithRedirects(Page page)
      throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("generator", "backlinks");
    properties.put("prop", "info");
    properties.put("gbltitle", page.getTitle());
    properties.put("gbllimit", "max" /*"500"*/);
    ArrayList<Page> links = new ArrayList<Page>();
    boolean blcontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/pages/page");
        Element root = getRoot(properties, MAX_ATTEMPTS);
        List results = xpa.selectNodes(root);
        Iterator iter = results.iterator();
        links.ensureCapacity(links.size() + results.size());
        XPath xpaPageId = XPath.newInstance("./@pageid");
        XPath xpaNs = XPath.newInstance("./@ns");
        XPath xpaTitle = XPath.newInstance("./@title");
        //XPath xpaTouched = XPath.newInstance("./@touched");
        XPath xpaLastRevId = XPath.newInstance("./@lastrevid");
        //XPath xpaCounter = XPath.newInstance("./@counter");
        //XPath xpaLength = XPath.newInstance("./@length");
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          Page link = DataManager.getPage(
              page.getWikipedia(), xpaTitle.valueOf(currentNode), null);
          link.setNamespace(xpaNs.valueOf(currentNode));
          link.setPageId(xpaPageId.valueOf(currentNode));
          link.setRevisionId(xpaLastRevId.valueOf(currentNode));
          if (currentNode.getAttribute("redirect") != null) {
            link.addRedirect(page);
          }
          links.add(link);
        }
        XPath xpaContinue = XPath.newInstance("/api/query-continue/backlinks");
        XPath xpaBlContinue = XPath.newInstance("./@gblcontinue");
        results = xpaContinue.selectNodes(root);
        iter = results.iterator();
        blcontinue = false;
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          properties.remove("titles");
          blcontinue = true;
          properties.put("gblcontinue", xpaBlContinue.valueOf(currentNode));
          
        }
      } catch (JDOMException e) {
        log.error("Error backlinks+redirects for page " + page.getTitle(), e);
        throw new APIException("Error parsing XML result", e);
      }
    } while (blcontinue);
    Collections.sort(links);
    page.setBackLinks(links);
  }

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * 
   * @param page Page.
   * @throws APIException
   */
  public void retrieveEmbeddedIn(Page page) throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("list", "embeddedin");
    properties.put("eititle", page.getTitle());
    properties.put("eilimit", "max" /*"500"*/);
    ArrayList<Page> links = new ArrayList<Page>();
    boolean eicontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/embeddedin/ei");
        Element root = getRoot(properties, MAX_ATTEMPTS);
        List results = xpa.selectNodes(root);
        Iterator iter = results.iterator();
        links.ensureCapacity(links.size() + results.size());
        XPath xpaPageId = XPath.newInstance("./@pageid");
        XPath xpaNs = XPath.newInstance("./@ns");
        XPath xpaTitle = XPath.newInstance("./@title");
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          Page link = DataManager.getPage(
              page.getWikipedia(), xpaTitle.valueOf(currentNode), null);
          link.setNamespace(xpaNs.valueOf(currentNode));
          link.setPageId(xpaPageId.valueOf(currentNode));
          links.add(link);
        }
        XPath xpaContinue = XPath.newInstance("/api/query-continue/embeddedin");
        XPath xpaBlContinue = XPath.newInstance("./@eicontinue");
        results = xpaContinue.selectNodes(root);
        iter = results.iterator();
        eicontinue = false;
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          properties.remove("eititle");
          eicontinue = true;
          properties.put("blcontinue", xpaBlContinue.valueOf(currentNode));
          
        }
      } catch (JDOMException e) {
        log.error("Error backlinks for page " + page.getTitle(), e);
        throw new APIException("Error parsing XML result", e);
      }
    } while (eicontinue);
    Collections.sort(links);
    page.setEmbeddedIn(links);
  }

  /**
   * Retrieves the templates of <code>page</code>.
   * 
   * @param page The page.
   */
  public void retrieveTemplates(Page page)
      throws APIException {
    ArrayList<Page> templates = new ArrayList<Page>();
    ArrayList<Page> newTemplates = new ArrayList<Page>();
    newTemplates.add(page);
    StringBuffer titles = new StringBuffer();
    do {
      HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
      titles.setLength(0);
      int count = 0;
      while ((count < MAX_PAGES_PER_QUERY) && !newTemplates.isEmpty()) {
        Page p = newTemplates.remove(0);
        if (count > 0) {
          titles.append("|");
        }
        titles.append(p.getTitle());
        if (!p.getTitle().equals(page.getTitle())) {
          templates.add(p);
        }
        count++;
      }
      properties.put("titles", titles.toString());
      properties.put("prop", "templates");
      properties.put("tllimit", "max" /*"1000"*/);
      try {
        constructTemplates(
            page.getWikipedia(),
            newTemplates, templates,
            getRoot(properties, MAX_ATTEMPTS),
            "/api/query/pages/page/templates/tl");
      } catch (JDOMParseException e) {
        log.error("Error retrieving templates", e);
        throw new APIException("Error parsing XML", e);
      }
    } while (!newTemplates.isEmpty());
    Collections.sort(templates);
    page.setTemplates(templates);
  }

  /**
   * Initialize the informations concerning redirects.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(ArrayList<Page> pages) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("redirects", "");
    StringBuffer titles = new StringBuffer();
    for (int i = 0; i < pages.size();) {
      titles.setLength(0);
      for (int j = 0; (j < MAX_PAGES_PER_QUERY) && (i < pages.size()); i++, j++) {
        Page p = pages.get(i);
        if (j > 0) {
          titles.append("|");
        }
        titles.append(p.getTitle());
      }
      properties.put("titles", titles.toString());
      try {
        updateRedirectStatus(
            pages,
            getRoot(properties, MAX_ATTEMPTS),
            "/api/query/redirects/r",
            "/api/query/pages");
      } catch (JDOMParseException e) {
        log.error("Error retrieving redirects", e);
        throw new APIException("Error parsing XML", e);
      }
    }
  }

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeDisambiguationStatus(ArrayList<Page> pages)
      throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("prop", "templates");
    properties.put("tllimit", "max");
    ArrayList<Page> tmpPages = new ArrayList<Page>();
    for (int i = 0; i < pages.size(); i++) {
      Iterator<Page> iter = pages.get(i).getRedirectIteratorWithPage();
      while (iter.hasNext()) {
        Page page = iter.next();
        tmpPages.add(page);
      }
    }
    StringBuffer titles = new StringBuffer();
    for (int i = 0; i < tmpPages.size();) {
      titles.setLength(0);
      for (int j = 0; (j < MAX_PAGES_PER_QUERY) && (i < tmpPages.size()); i++, j++) {
        Page p = tmpPages.get(i);
        if (j > 0) {
          titles.append("|");
        }
        titles.append(p.getTitle());
        p.setDisambiguationPage(null);
      }
      properties.put("titles", titles.toString());
      try {
        boolean tlcontinue = false;
        do {
          Element root = getRoot(properties, MAX_ATTEMPTS);
          updateDisambiguationStatus(
              tmpPages, root,
              "/api/query/pages/page");
          XPath xpaContinue = XPath.newInstance("/api/query-continue/templates");
          XPath xpaTlContinue = XPath.newInstance("./@tlcontinue");
          List results = xpaContinue.selectNodes(root);
          Iterator iter = results.iterator();
          tlcontinue = false;
          while (iter.hasNext()) {
            Element currentNode = (Element) iter.next();
            tlcontinue = true;
            properties.put("tlcontinue", xpaTlContinue.valueOf(currentNode));
          }
        } while (tlcontinue);
      } catch (JDOMParseException e) {
        log.error("Error retrieving disambiguation status", e);
        throw new APIException("Error parsing XML", e);
      } catch (JDOMException e) {
        log.error("Error retrieving disambiguation status", e);
        throw new APIException("Error parsing XML", e);
      }
    }
  }

  /**
   * Load namespaces informations.
   * 
   * @throws APIException
   */
  private void loadNamespaces() throws APIException {
    HashMap<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("meta", "siteinfo");
    properties.put("siprop", "namespaces");
    try {
      namespaces = constructNamespaces(
          getRoot(properties, MAX_ATTEMPTS),
          "/api/query/namespaces/ns");
    } catch (JDOMParseException e) {
      log.error("Error loading namespaces", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * @param root Root element in MediaWiki answer.
   * 
   * @param query Path to the answer.
   * @return Result of the login.
   * @throws APIException
   */
  private LoginResult constructLogin(Element root, String query)
      throws APIException {
    try {
      XPath xpa = XPath.newInstance(query);
      Element node = (Element) xpa.selectSingleNode(root);
      if (node != null) {
        XPath xpaResult = XPath.newInstance("./@result");
        String result = xpaResult.valueOf(node);
        if ("Success".equalsIgnoreCase(result)) {
          XPath xpaUserid = XPath.newInstance("./@lguserid");
          XPath xpaUsername = XPath.newInstance("./@lgusername");
          XPath xpaToken = XPath.newInstance("./@lgtoken");
          lguserid = xpaUserid.valueOf(node);
          lgusername = xpaUsername.valueOf(node);
          lgtoken = xpaToken.valueOf(node);
          return LoginResult.createCorrectLogin();
        }
        XPath xpaWait = XPath.newInstance("./@wait");
        XPath xpaDetails = XPath.newInstance("./@details");
        return LoginResult.createErrorLogin(result, xpaDetails.valueOf(node), xpaWait.valueOf(node));
      }
    } catch (JDOMException e) {
      log.error("Error login", e);
      throw new APIException("Error parsing XML result", e);
    }
    return LoginResult.createErrorLogin(null, null, null);
  }

  /**
   * @param root Root element in MediaWiki answer.
   * 
   * @param query Path to the answer.
   * @return Result of the query.
   * @throws APIException
   */
  private QueryResult constructEdit(Element root, String query)
      throws APIException {
    try {
      XPath xpa = XPath.newInstance(query);
      Element node = (Element) xpa.selectSingleNode(root);
      if (node != null) {
        XPath xpaResult = XPath.newInstance("./@result");
        String result = xpaResult.valueOf(node);
        if ("Success".equalsIgnoreCase(result)) {
          return QueryResult.createCorrectQuery();
        }
        XPath xpaWait = XPath.newInstance("./@wait");
        XPath xpaDetails = XPath.newInstance("./@details");
        return QueryResult.createErrorQuery(result, xpaDetails.valueOf(node), xpaWait.valueOf(node));
      }
    } catch (JDOMException e) {
      log.error("Error login", e);
      throw new APIException("Error parsing XML result", e);
    }
    return QueryResult.createErrorQuery(null, null, null);
  }

  /**
   * @param root Root element.
   * @param query XPath query to retrieve the namespaces 
   * @return List of namespaces.
   * @throws APIException
   */
  private HashMap<Integer, Namespace> constructNamespaces(Element root, String query)
      throws APIException {
    HashMap<Integer, Namespace> list = null;
    try {
      XPath xpa = XPath.newInstance(query);
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      list = new HashMap<Integer, Namespace>();
      XPath xpaId = XPath.newInstance("./@id");
      XPath xpaTitle = XPath.newInstance(".");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Namespace ns = new Namespace(
            xpaId.valueOf(currentNode),
            xpaTitle.valueOf(currentNode));
        list.put(ns.getId(), ns);
      }
    } catch (JDOMException e) {
      log.error("Error namespaces", e);
      throw new APIException("Error parsing XML result", e);
    }
    return list;
  }

  /**
   * @param page Page.
   * @param root Root element.
   * @param query XPath query to retrieve the links 
   * @throws APIException
   */
  private void constructLinks(Page page, Element root, String query)
      throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    ArrayList<Page> links = null;
    try {
      XPath xpa = XPath.newInstance(query);
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      links = new ArrayList<Page>(results.size());
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Page link = DataManager.getPage(
            page.getWikipedia(), xpaTitle.valueOf(currentNode), null);
        link.setNamespace(xpaNs.valueOf(currentNode));
        links.add(link);
      }
    } catch (JDOMException e) {
      log.error("Error links for page " + page.getTitle(), e);
      throw new APIException("Error parsing XML result", e);
    }
    page.setLinks(links);
  }

  /**
   * @param page Page.
   * @param root Root element.
   * @param query XPath query to retrieve the links 
   * @throws APIException
   */
  private void constructLinksWithRedirects(
      Page page, Element root, String query,
      ArrayList<Page> redirects,
      boolean keepExistingLinks)
      throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    ArrayList<Page> links = null;
    try {
      XPath xpa = XPath.newInstance(query);
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      if (keepExistingLinks) {
        links = page.getLinks();
      } else {
        links = new ArrayList<Page>(results.size());
      }
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      XPath xpaRevisionId = XPath.newInstance("./@lastrevid");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        Page link = DataManager.getPage(
            page.getWikipedia(),
            xpaTitle.valueOf(currentNode),
            xpaRevisionId.valueOf(currentNode));
        link.setNamespace(xpaNs.valueOf(currentNode));
        if (currentNode.getAttribute("pageid") != null) {
          link.setExisting(Boolean.TRUE);
        } else if (currentNode.getAttribute("missing") != null) {
          link.setExisting(Boolean.FALSE);
        }
        if ((currentNode.getAttribute("redirect") != null) && (redirects != null)) {
          redirects.add(link);
        }
        links.add(link);
      }
    } catch (JDOMException e) {
      log.error("Error links for page " + page.getTitle(), e);
      throw new APIException("Error parsing XML result", e);
    }
    page.setLinks(links);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param newTemplates New list of templates (where to add new ones).
   * @param templates List of already found templates.
   * @param root Root element.
   * @param query XPath query to retrieve the links 
   * @throws APIException
   */
  private void constructTemplates(
      EnumWikipedia wikipedia,
      ArrayList<Page> newTemplates, ArrayList<Page> templates,
      Element root, String query)
      throws APIException {
    if ((newTemplates == null) || (templates == null)) {
      throw new APIException("Templates are null");
    }
    try {
      XPath xpa = XPath.newInstance(query);
      List results = xpa.selectNodes(root);
      Iterator iter = results.iterator();
      XPath xpaNs = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");
      while (iter.hasNext()) {
        Element currentNode = (Element) iter.next();
        String namespace = xpaNs.valueOf(currentNode);
        String title = xpaTitle.valueOf(currentNode);
        if (title != null) {
          boolean alreadyFound = false;
          for (Page p : newTemplates) {
            if (title.equals(p.getTitle())) {
              alreadyFound = true;
              break;
            }
          }
          for (Page p : templates) {
            if (title.equals(p.getTitle())) {
              alreadyFound = true;
              break;
            }
          }
          if (!alreadyFound) {
            Page template = DataManager.getPage(wikipedia, title, null);
            template.setNamespace(namespace);
            newTemplates.add(template);
          }
        }
      }
    } catch (JDOMException e) {
      log.error("Error templates", e);
      throw new APIException("Error parsing XML result", e);
    }
  }

  /**
   * @param page Page.
   * @param root Root element.
   * @param query XPath query to retrieve the contents 
   * @throws JDOMException
   */
  private boolean constructContents(Page page, Element root, String query)
      throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    boolean redirect = false;
    try {
      XPath xpaPage = XPath.newInstance(query);
      Element node = (Element) xpaPage.selectSingleNode(root);
      if (node != null) {
        XPath xpaNamespace = XPath.newInstance("./@ns");
        page.setNamespace(xpaNamespace.valueOf(node));
        if (node.getAttribute("redirect") != null) {
          redirect = true;
        }
        XPath xpaEditToken = XPath.newInstance("./@edittoken");
        page.setEditToken(xpaEditToken.valueOf(node));
      }
      XPath xpa = XPath.newInstance(query + "/revisions/rev");
      node = (Element) xpa.selectSingleNode(root);
      if (node != null) {
        XPath xpaContents = XPath.newInstance(".");
        XPath xpaRevision = XPath.newInstance("./@revid");
        XPath xpaTimestamp = XPath.newInstance("./@timestamp");
        page.setContents(xpaContents.valueOf(node));
        page.setRevisionId(xpaRevision.valueOf(node));
        page.setContentsTimestamp(xpaTimestamp.valueOf(node));
      }
    } catch (JDOMException e) {
      log.error("Error contents for page " + page.getTitle(), e);
      throw new APIException("Error parsing XML result", e);
    }
    return redirect;
  }

  /**
   * Update redirect information of a list of pages.
   * 
   * @param pages List of pages.
   * @param root Root element.
   * @param queryRedirects XPath query to retrieve the redirects.
   * @param queryPages XPath query to retrieve the pages.
   * @throws APIException
   */
  private void updateRedirectStatus(
      ArrayList<Page> pages,
      Element root,
      String queryRedirects,
      String queryPages)
      throws APIException {
    try {
      XPath xpaRedirects = XPath.newInstance(queryRedirects);
      XPath xpaPages = XPath.newInstance(queryPages);
      List listRedirects = xpaRedirects.selectNodes(root);
      Element listPages = (Element) xpaPages.selectSingleNode(root);
      Iterator iterRedirects = listRedirects.iterator();
      XPath xpaFrom = XPath.newInstance("./@from");
      XPath xpaTo = XPath.newInstance("./@to");
      XPath xpaPageId = XPath.newInstance("./@pageid");
      XPath xpaNamespace = XPath.newInstance("./@ns");
      XPath xpaTitle = XPath.newInstance("./@title");

      // Analyzing redirects
      while (iterRedirects.hasNext()) {
        Element currentRedirect = (Element) iterRedirects.next();
        String fromPage = xpaFrom.valueOf(currentRedirect);
        String toPage = xpaTo.valueOf(currentRedirect);
        for (Page p : pages) {
          boolean exist = false;
          Iterator<Page> iter = p.getRedirectIteratorWithPage();
          while (iter.hasNext()) {
            Page tmp = iter.next();
            if ((tmp.getTitle() != null) && (tmp.getTitle().equals(toPage))) {
              exist = true;
            }
            if (!exist &&
                (tmp.getTitle() != null) &&
                (tmp.getTitle().equals(fromPage))) {
              XPath xpaPage = createXPath("page", "title", toPage);
              List listTo = xpaPage.selectNodes(listPages);
              if (!listTo.isEmpty()) {
                Element to = (Element) listTo.get(0);
                Page pageTo = DataManager.getPage(
                    p.getWikipedia(), xpaTitle.valueOf(to), null);
                pageTo.setNamespace(xpaNamespace.valueOf(to));
                pageTo.setPageId(xpaPageId.valueOf(to));
                p.addRedirect(pageTo);
              }
            }
          }
        }
      }

      // Analyzing missing pages
      XPath xpaMissing = XPath.newInstance("./@missing");
      for (Page p : pages) {
        Iterator<Page> iter = p.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page tmp = iter.next();
          XPath xpaPage = createXPath("page", "title", tmp.getTitle());
          Element page = (Element) xpaPage.selectSingleNode(listPages);
          if (page != null) {
            List pageId = xpaPageId.selectNodes(page);
            if ((pageId != null) && (!pageId.isEmpty())) {
              tmp.setExisting(Boolean.TRUE);
            } else {
              List missing = xpaMissing.selectNodes(page);
              if ((missing != null) && (!missing.isEmpty())) {
                tmp.setExisting(Boolean.FALSE);
              }
            }
          }
        }
      }
    } catch (JDOMException e) {
      log.error("Error redirects", e);
      throw new APIException("Error parsing XML result", e);
    }
  }

  /**
   * Update disambiguation information of a list of pages.
   * 
   * @param pages List of pages.
   * @param root Root element.
   * @param query XPath query to retrieve the list of templates.
   * @throws APIException
   */
  private void updateDisambiguationStatus(
      ArrayList<Page> pages,
      Element root,
      String query)
      throws APIException {
    try {
      XPath xpa = XPath.newInstance(query);
      List listPages = xpa.selectNodes(root);
      Iterator iterPages = listPages.iterator();
      XPath xpaTitle = XPath.newInstance("./@title");
      XPath xpaTemplate = createXPath("templates/tl", "ns", "" + Namespace.TEMPLATE);
      XPath xpaTemplateName = XPath.newInstance("./@title");
      while (iterPages.hasNext()) {
        Element currentNode = (Element) iterPages.next();
        String title = xpaTitle.valueOf(currentNode);
        for (Page p : pages) {
          if ((p.getTitle() != null) &&
              (p.getTitle().equals(title))) {
            Boolean disambiguation = Boolean.FALSE;
            Boolean wiktionaryLink = Boolean.FALSE;
            List listTemplates = xpaTemplate.selectNodes(currentNode);
            Iterator iterTemplates = listTemplates.iterator();
            while (iterTemplates.hasNext()) {
              Element currentTemplate = (Element) iterTemplates.next();
              String templateName = xpaTemplateName.valueOf(currentTemplate);
              if (mediawikiAPI.isDisambiguationTemplate(templateName, this)) {
                disambiguation = Boolean.TRUE;
              }
              if (mediawikiAPI.isWiktionaryTemplate(templateName)) {
                wiktionaryLink = Boolean.TRUE;
              }
            }
            if ((p.isDisambiguationPage() == null) ||
                (Boolean.TRUE.equals(disambiguation))) {
              p.setDisambiguationPage(disambiguation);
            }
            p.setWiktionaryLink(wiktionaryLink);
          }
        }
      }
    } catch (JDOMException e) {
      log.error("Error disambiguation", e);
      throw new APIException("Error parsing XML result", e);
    }
  }

  /**
   * Returns an initialized set of properties.
   * 
   * @param action Action called in the MediaWiki API.
   * @param newApi New API (api.php) or not (query.php).
   * @return Properties.
   */
  private HashMap<String, String> getProperties(
      String action,
      boolean newApi) {
    HashMap<String, String> properties = new HashMap<String, String>();
    properties.put(newApi ? "action" : "what", action);
    properties.put("format", "xml");
    return properties;
  }

  /**
   * Returns the root element of the XML document returned by MediaWiki API.
   * 
   * @param properties Properties to drive the API.
   * @param maxTry Maximum number of tries.
   * @return Root element.
   * @throws APIException
   */
  private Element getRoot(
      HashMap<String, String> properties,
      int                     maxTry)
      throws JDOMParseException, APIException {
    Element root = null;
    PostMethod method = null;
    int attempt = 0;
    do {
      attempt++;
      try {
        try {
          String url = mediawikiAPI.getApiURL();
          StringBuffer debugUrl = (DEBUG_URL) ? new StringBuffer(url) : null;
          method = new PostMethod(url);
          method.getParams().setContentCharset("UTF-8");
          method.setRequestHeader("Accept-Encoding", "gzip");
          if (properties != null) {
            boolean first = true;
            Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
            while (iter.hasNext()) {
              Map.Entry<String, String> property = iter.next();
              String key = property.getKey();
              String value = property.getValue();
              method.addParameter(key, value);
              if (debugUrl != null) {
                if (value.indexOf('\n') > 0) {
                  value = value.substring(0, value.indexOf('\n')) + "...";
                }
                debugUrl.append(
                    (first ? "?" : "&") +
                    key + "=" +
                    ("lgpassword".equals(key) ? "XXXXX" : value));
                first = false;
              }
            }
            if (debugUrl != null) {
              if (DEBUG_TIME) {
                System.out.println("" + System.currentTimeMillis() + ": " + debugUrl.toString());
              } else {
                System.out.println(debugUrl.toString());
              }
            }
          }
          if (lgtoken != null) {
            method.addParameter("lgtoken", lgtoken);
          }
          if (lgusername != null) {
            method.addParameter("lgusername", lgusername);
          }
          if (lguserid != null) {
            method.addParameter("lguserid", lguserid);
          }
          int statusCode = httpClient.executeMethod(method);
          if (statusCode != HttpStatus.SC_OK) {
            throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
          }
          InputStream stream = method.getResponseBodyAsStream();
          stream = new BufferedInputStream(stream);
          Header contentEncoding = method.getResponseHeader("Content-Encoding");
          if (contentEncoding != null) {
            if (contentEncoding.getValue().equals("gzip")) {
              stream = new GZIPInputStream(stream);
            }
          }
          SAXBuilder sxb = new SAXBuilder();
          Document document = sxb.build(stream);
          traceDocument(document);
          root = document.getRootElement();
          checkForError(root);
          return root;
        } catch (JDOMParseException e) {
          // NOTE: to deal with api.php login action being disabled.
          log.error("JDOMParseException: " + e.getMessage());
          throw e;
        } catch (JDOMException e) {
          log.error("JDOMException: " + e.getMessage());
          throw new APIException("Error parsing XML result", e);
        } catch (IOException e) {
          log.error("IOException: " + e.getMessage());
          throw new APIException("Error accessing MediaWiki", e);
        } finally {
          if (method != null) {
            method.releaseConnection();
          }
        }
      } catch (JDOMParseException e) {
        if (attempt >= maxTry) {
          log.warn("Error. Maximum attemps count reached.");
          throw e;
        }
        log.warn("Error. Trying again");
      } catch (APIException e) {
        if (attempt >= maxTry) {
          log.warn("Error. Maximum attemps count reached.");
          throw e;
        }
        log.warn("Error. Trying again");
      }
    } while (attempt < maxTry);
    return root;
  }

  /**
   * Check for errors reported by the API.
   * 
   * @param root Document root.
   */
  private void checkForError(Element root) {
    if (root == null) {
      return;
    }
    
    // Check for errors
    try {
      XPath xpa = XPath.newInstance("/api/error");
      List listErrors = xpa.selectNodes(root);
      if (listErrors != null) {
        Iterator iterErrors = listErrors.iterator();
        XPath xpaCode = XPath.newInstance("./@code");
        XPath xpaInfo = XPath.newInstance("./@info");
        while (iterErrors.hasNext()) {
          Element currentNode = (Element) iterErrors.next();
          log.warn("Error reported: " + xpaCode.valueOf(currentNode) + " - " + xpaInfo.valueOf(currentNode));
        }
      }
    } catch (JDOMException e) {
      log.error("JDOMException: " + e.getMessage());
    }
    
    // Check for warnings
    try {
      XPath xpa = XPath.newInstance("/api/warnings/*");
      List listWarnings = xpa.selectNodes(root);
      if (listWarnings != null) {
        Iterator iterWarnings = listWarnings.iterator();
        while (iterWarnings.hasNext()) {
          Element currentNode = (Element) iterWarnings.next();
          log.warn("Warning reported: " + currentNode.getName() + " - " + currentNode.getValue());
        }
      }
    } catch( JDOMException e) {
      log.error("JDOMException: " + e.getMessage());
    }
  }

  /**
   * Utility method to create a XPath.
   * 
   * @param element Element.
   * @param attribute Attribute.
   * @param value Attribute value.
   * @return XPath
   * @throws JDOMException
   */
  private static XPath createXPath(
      String element,
      String attribute,
      String value)
      throws JDOMException {
    if ((value != null) && (value.indexOf("'") != -1)) {
      return XPath.newInstance(element + "[@" + attribute + "=\"" + value + "\"]");
    }
    return XPath.newInstance(element + "[@" + attribute + "='" + value + "']");
  }

  /**
   * Trace a document contents.
   * 
   * @param doc Document.
   */
  private void traceDocument(Document doc) {
    if (DEBUG_XML) {
      if (xmlOutputter == null) {
        xmlOutputter = new XMLOutputter(Format.getPrettyFormat());
      }
      try {
        System.out.println("********** START OF DOCUMENT **********");
        xmlOutputter.output(doc, System.out);
        System.out.println("**********  END OF DOCUMENT  **********");
      } catch (IOException e) {
        // Nothing to do
      }
    }
  }
}
