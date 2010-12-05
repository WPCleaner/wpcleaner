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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.params.HttpMethodParams;
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
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.MagicWord;
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
  private static XMLOutputter xmlOutputter = new XMLOutputter(Format.getPrettyFormat());

  private HttpClient httpClient;

  private String username = null;
  private String password = null;
  private String lgtoken = null;
  private String lgusername = null;
  private String lguserid = null;

  /**
   * Constructor.
   * @param manager HTTP connection manager.
   */
  public MediaWikiAPI(HttpConnectionManager manager) {
    httpClient = new HttpClient(manager);
    httpClient.getParams().setParameter(
        HttpMethodParams.USER_AGENT,
        "WikiCleaner (+http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation)");
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
   * @param wikipedia Wikipedia.
   * @param username1 User name.
   * @param password1 Password.
   * @param login Flag indicating if login should be done.
   * @return Login status.
   */
  public LoginResult login(
      EnumWikipedia wikipedia,
      String username1,
      String password1,
      boolean login) throws APIException {

    // Login
    this.username = username1;
    this.password = password1;
    logout();
    lgtoken = null;
    lgusername = null;
    lguserid = null;
    LoginResult result = null;
    if (login) {
      Map<String, String> properties = getProperties(ACTION_API_LOGIN, true);
      properties.put("lgname", username);
      properties.put("lgpassword", password);
      try {
        result = constructLogin(
            getRoot(wikipedia, properties, 1),
            "/api/login");
        if ((result != null) && (result.isTokenNeeded())) {
          properties.put("lgtoken", result.getDetails());
          result = constructLogin(
              getRoot(wikipedia, properties, 1),
              "/api/login");
        }
      } catch (JDOMParseException e) {
        log.error("Exception in MediaWikiAPI.login()", e);
        throw new APIException("Couldn't login");
      }
    }

    // Retrieve configuration
    if (wikipedia.getConfiguationPage() != null) {
      Page page = DataManager.getPage(
          wikipedia, wikipedia.getConfiguationPage(), null, null);
      retrieveContents(wikipedia, page, false);
      wikipedia.initConfiguration(page.getContents());
    }

    // Retrieve data
    loadSiteInfo(wikipedia);

    return result;
  }

  /**
   * Logout.
   */
  public void logout() {
    this.lgtoken = null;
    this.lgusername = null;
    this.lguserid = null;
  }

  /**
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param parameters Request parameters.
   * @param stream Flag indicating if the stream is needed.
   * @return Answer.
   * @throws APIException
   */
  public InputStream askToolServerPost(
      String          path,
      NameValuePair[] parameters,
      boolean         stream) throws APIException {
    try {
      String url = "http://toolserver.org/" + path;
      StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder(url) : null;
      PostMethod method = new PostMethod(url);
      method.getParams().setContentCharset("UTF-8");
      method.setRequestHeader("Accept-Encoding", "gzip");
      if (parameters != null) {
        method.addParameters(parameters);
      }
      if (DEBUG_URL) {
        if (parameters != null) {
          for (int i = 0; i < parameters.length; i++) {
            debugUrl.append(
                (i == 0 ? "?" : "&") +
                parameters[i].getName() + "=" + parameters[i].getValue());
          }
        }
        if (DEBUG_TIME) {
          System.out.println("" + System.currentTimeMillis() + ": " + debugUrl.toString());
        } else {
          System.out.println(debugUrl.toString());
        }
      }
      HttpClient toolClient = new HttpClient(new MultiThreadedHttpConnectionManager());
      int statusCode = toolClient.executeMethod(method);
      if (statusCode != HttpStatus.SC_OK) {
        throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
      }
      if (!stream) {
        return null;
      }
      InputStream inputStream = method.getResponseBodyAsStream();
      inputStream = new BufferedInputStream(inputStream);
      Header contentEncoding = method.getResponseHeader("Content-Encoding");
      if (contentEncoding != null) {
        if (contentEncoding.getValue().equals("gzip")) {
          inputStream = new GZIPInputStream(inputStream);
        }
      }
      return inputStream;
    } catch (HttpException e) {
      throw new APIException("HttpException: " + e.getMessage());
    } catch (IOException e) {
      throw new APIException("IOException: " + e.getMessage());
    }
  }

  /**
   * Send a GET request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param stream Flag indicating if the stream is needed.
   * @return Answer.
   * @throws APIException
   */
  public InputStream askToolServerGet(
      String          path,
      boolean         stream) throws APIException {
    try {
      String url = "http://toolserver.org/" + path;
      StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder(url) : null;
      GetMethod method = new GetMethod(url);
      method.getParams().setContentCharset("UTF-8");
      method.setRequestHeader("Accept-Encoding", "gzip");
      if (DEBUG_URL) {
        if (DEBUG_TIME) {
          System.out.println("" + System.currentTimeMillis() + ": " + debugUrl.toString());
        } else {
          System.out.println(debugUrl.toString());
        }
      }
      HttpClient toolClient = new HttpClient(new MultiThreadedHttpConnectionManager());
      int statusCode = toolClient.executeMethod(method);
      if (statusCode == HttpStatus.SC_NOT_FOUND) {
        return null;
      }
      if (statusCode != HttpStatus.SC_OK) {
        throw new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
      }
      if (!stream) {
        return null;
      }
      InputStream inputStream = method.getResponseBodyAsStream();
      inputStream = new BufferedInputStream(inputStream);
      Header contentEncoding = method.getResponseHeader("Content-Encoding");
      if (contentEncoding != null) {
        if (contentEncoding.getValue().equals("gzip")) {
          inputStream = new GZIPInputStream(inputStream);
        }
      }
      return inputStream;
    } catch (HttpException e) {
      throw new APIException("HttpException: " + e.getMessage());
    } catch (IOException e) {
      throw new APIException("IOException: " + e.getMessage());
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @return The title of a random page.
   * @throws APIException
   */
  //TODO: Use api.php: action=query&list=random
  public String getRandomPage(EnumWikipedia wikipedia) throws APIException {
    BufferedReader in = null;
    try {
      String url = wikipedia.getWikiURL() + "?title=Special:Random";
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
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   */
  public void retrieveContents(EnumWikipedia wikipedia, Page page, boolean withRedirects)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("inprop", "protection");
    properties.put("prop", "revisions|info");
    properties.put("titles", page.getTitle());
    properties.put("rvprop", "content|ids|timestamp");
    if (lgtoken != null) {
      properties.put("intoken", "edit");
    }
    try {
      boolean redirect = constructContents(
          page,
          getRoot(wikipedia, properties, MAX_ATTEMPTS),
          "/api/query/pages/page");
      if (redirect && withRedirects) {
        List<Page> pages = new ArrayList<Page>(1);
        pages.add(page);
        initializeRedirect(wikipedia, pages);
      }
    } catch (JDOMParseException e) {
      log.error("Error retrieving page content", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Retrieves the contents of a section in a <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param section Section number.
   * @throws APIException
   */
  public void retrieveSectionContents(EnumWikipedia wikipedia, Page page, int section)
    throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("prop", "revisions|info");
    properties.put("titles", page.getTitle());
    properties.put("rvprop", "content|ids|timestamp");
    properties.put("rvsection", Integer.toString(section));
    if (lgtoken != null) {
      properties.put("intoken", "edit");
    }
    try {
      constructContents(
          page,
          getRoot(wikipedia, properties, MAX_ATTEMPTS),
          "/api/query/pages/page");
    } catch (JDOMParseException e) {
      log.error("Error retrieving page content", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void retrieveContentsWithoutRedirects(EnumWikipedia wikipedia, List<Page> pages)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("prop", "revisions");
    properties.put("rvprop", "content");
    StringBuilder titles = new StringBuilder();
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
        constructContents(
            pages,
            getRoot(wikipedia, properties, MAX_ATTEMPTS),
            "/api/query/pages/page");
      } catch (JDOMParseException e) {
        log.error("Error retrieving redirects", e);
        throw new APIException("Error parsing XML", e);
      }
    }
  }
  
  /**
   * Expand templates in a text.
   * 
   * @param wikipedia Wikipedia.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Text with templates expanded.
   * @throws APIException
   */
  public String expandTemplates(EnumWikipedia wikipedia, String title, String text) throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_EXPAND, true);
    properties.put("title", title);
    properties.put("text", text);
    try {
      XPath xpaContents = XPath.newInstance("/api/expandtemplates/.");
      Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
      return xpaContents.valueOf(root);
    } catch (JDOMException e) {
      log.error("Error expanding templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Parse text.
   * 
   * @param wikipedia Wikipedia.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Parsed text.
   * @throws APIException
   */
  public String parseText(EnumWikipedia wikipedia, String title, String text) throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_PARSE, true);
    properties.put("title", title);
    properties.put("text", text);
    try {
      XPath xpaContents = XPath.newInstance("/api/parse/text/.");
      Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
      return xpaContents.valueOf(root);
    } catch (JDOMException e) {
      log.error("Error expanding templates", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Update a page on Wikipedia.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param newContents New contents to use.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updatePage(
      EnumWikipedia wikipedia, Page page,
      String newContents, String comment,
      boolean forceWatch) throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    if (newContents == null) {
      throw new APIException("Contents is null");
    }
    if (comment == null) {
      throw new APIException("Comment is null");
    }
    if (lgtoken == null) {
      throw new APIException("You must be logged in to update pages");
    }
    QueryResult result = null;
    Map<String, String> properties = getProperties(ACTION_API_EDIT, true);
    properties.put("assert", "user");
    if (page.getContentsTimestamp() != null) {
      properties.put("basetimestamp", page.getContentsTimestamp());
    }
    properties.put("bot", "");
    properties.put("minor", "");
    if (page.getStartTimestamp() != null) {
      properties.put("starttimestamp", page.getStartTimestamp());
    }
    properties.put("summary", comment);
    properties.put("text", newContents);
    properties.put("title", page.getTitle());
    properties.put("token", page.getEditToken());
    properties.put("watchlist", forceWatch ? "watch" : "nochange");
    try {
      result = constructEdit(
          getRoot(wikipedia, properties, 1),
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
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param title Title of the new section.
   * @param contents Contents.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult addNewSection(
      EnumWikipedia wikipedia,
      Page page, String title, String contents, boolean forceWatch) throws APIException {
    return updateSection(wikipedia, page, title, "new", contents, forceWatch);
  }

  /**
   * Update a section in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param title Title of the new section.
   * @param section Section. 
   * @param contents Contents.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  public QueryResult updateSection(
      EnumWikipedia wikipedia,
      Page page, String title, int section,
      String contents, boolean forceWatch) throws APIException {
    return updateSection(wikipedia, page, title, Integer.toString(section), contents, forceWatch);
  }

  /**
   * Update a section or create a new section in a page.
   * 
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param title Title of the new section.
   * @param section Section ("new" for a new section). 
   * @param contents Contents.
   * @param forceWatch Force watching the page.
   * @return Result of the command.
   * @throws APIException
   */
  private QueryResult updateSection(
      EnumWikipedia wikipedia,
      Page page, String title, String section,
      String contents, boolean forceWatch) throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    if (title == null) {
      throw new APIException("Title is null");
    }
    if (contents == null) {
      throw new APIException("Contents is null");
    }
    if (lgtoken == null) {
      throw new APIException("You must be logged in to update pages");
    }
    QueryResult result = null;
    Map<String, String> properties = getProperties(ACTION_API_EDIT, true);
    properties.put("assert", "user");
    if (page.getContentsTimestamp() != null) {
      properties.put("basetimestamp", page.getContentsTimestamp());
    }
    properties.put("bot", "");
    properties.put("minor", "");
    properties.put("section", section);
    if (page.getStartTimestamp() != null) {
      properties.put("starttimestamp", page.getStartTimestamp());
    }
    properties.put("summary", title);
    properties.put("text", contents);
    properties.put("title", page.getTitle());
    properties.put("token", page.getEditToken());
    properties.put("watchlist", forceWatch ? "watch" : "nochange");
    try {
      result = constructEdit(
          getRoot(wikipedia, properties, 1),
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
   * @param wikipedia Wikipedia.
   * @param page The page.
   */
  public void purgePageCache(EnumWikipedia wikipedia, Page page)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_PURGE, true);
    properties.put("titles", page.getTitle());
    try {
      checkForError(getRoot(wikipedia, properties, MAX_ATTEMPTS));
    } catch (JDOMParseException e) {
      log.error("Error purging page cache", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * @param from Wikipedia in which the article is.
   * @param to Wikipedia to which the link is searched.
   * @param title Page title.
   * @return Page title in the destination Wikipedia.
   * @throws APIException
   */
  public String getLanguageLink(EnumWikipedia from, EnumWikipedia to, String title)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("lllimit", "max");
    properties.put("prop", "langlinks");
    properties.put("titles", title);
    boolean llcontinue = false;
    do {
      try {
        Element root = getRoot(from, properties, MAX_ATTEMPTS);

        // Analyzing result
        XPath xpaLangLink = XPath.newInstance("/api/query/pages/page/langlinks/ll[@lang=\"" + to.getCode() + "\"]");
        List results = xpaLangLink.selectNodes(root);
        Iterator iter = results.iterator();
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          XPath xpaLink = XPath.newInstance(".");
          String link = xpaLink.valueOf(currentNode);
          if ((link != null) && (link.trim().length() > 0)) {
            return link.trim();
          }
        }

        // Checking if need to continue
        XPath xpaContinue = XPath.newInstance("/api/query-continue/langlinks");
        XPath xpaLlContinue = XPath.newInstance("./@llcontinue");
        results = xpaContinue.selectNodes(root);
        iter = results.iterator();
        llcontinue = false;
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          llcontinue = true;
          properties.put("llcontinue", xpaLlContinue.valueOf(currentNode));
        }
      } catch (JDOMException e) {
        log.error("Error retrieving language links", e);
        throw new APIException("Error retrieving language links", e);
      }
    } while (llcontinue);
    return null;
  }

  /**
   * Retrieves the links of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   */
  public void retrieveLinks(EnumWikipedia wikipedia, Page page)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("pllimit", "max");
    properties.put("prop", "links");
    properties.put("titles", page.getTitle());
    try {
      constructLinks(
          page,
          getRoot(wikipedia, properties, MAX_ATTEMPTS),
          "/api/query/pages/page/links/pl");
    } catch (JDOMParseException e) {
      log.error("Error retrieving page content", e);
      throw new APIException("Error parsing XML", e);
    }
  }

  /**
   * Retrieves the links of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @param namespace If set, retrieve only links in this namespace.
   * @param knownPages Already known pages.
   */
  public void retrieveLinksWithRedirects(
      EnumWikipedia wikipedia,
      Page page, Integer namespace, List<Page> knownPages)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("generator", "links");
    properties.put("gpllimit", "max");
    if (namespace != null) {
      properties.put("gplnamespace", namespace.toString());
    }
    properties.put("prop", "info");
    properties.put("titles", page.getTitle());
    boolean keepLinks = false;
    boolean gplcontinue = false;
    List<Page> redirects = new ArrayList<Page>();
    do {
      try {
        Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
        constructLinksWithRedirects(
            page, root,
            "/api/query/pages/page",
            knownPages,
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
      initializeRedirect(wikipedia, redirects);
      retrieveContentsWithoutRedirects(wikipedia, redirects);
    }
  }

  /**
   * Retrieves the back links of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   */
  public void retrieveBackLinks(EnumWikipedia wikipedia, Page page)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("list", "backlinks");
    properties.put("bltitle", page.getTitle());
    properties.put("bllimit", /*"max"*/ "500"); // TODO
    List<Page> links = new ArrayList<Page>();
    boolean blcontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/backlinks/bl");
        Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
        List results = xpa.selectNodes(root);
        Iterator iter = results.iterator();
        //links.ensureCapacity(links.size() + results.size());
        XPath xpaPageId = XPath.newInstance("./@pageid");
        XPath xpaNs = XPath.newInstance("./@ns");
        XPath xpaTitle = XPath.newInstance("./@title");
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          Page link = DataManager.getPage(
              page.getWikipedia(), xpaTitle.valueOf(currentNode), null, null);
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
   * @param wikipedia Wikipedia.
   * @param page The page.
   */
  public void retrieveBackLinksWithRedirects(EnumWikipedia wikipedia, Page page)
      throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("generator", "backlinks");
    properties.put("prop", "info");
    properties.put("gbltitle", page.getTitle());
    properties.put("gbllimit", /*"max"*/ "500"); // TODO
    List<Page> links = new ArrayList<Page>();
    boolean blcontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/pages/page");
        Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
        List results = xpa.selectNodes(root);
        Iterator iter = results.iterator();
        //links.ensureCapacity(links.size() + results.size());
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
              page.getWikipedia(), xpaTitle.valueOf(currentNode), null, null);
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
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param namespace Limit to namespace (optional).
   * @throws APIException
   */
  public void retrieveEmbeddedIn(
      EnumWikipedia wikipedia, Page page, Integer namespace) throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("list", "embeddedin");
    properties.put("eilimit", "max" /*"500"*/);
    if (namespace != null) {
      properties.put("einamespace", namespace.toString());
    }
    properties.put("eititle", page.getTitle());
    List<Page> links = new ArrayList<Page>();
    boolean eicontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/embeddedin/ei");
        Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
        List results = xpa.selectNodes(root);
        Iterator iter = results.iterator();
        //links.ensureCapacity(links.size() + results.size());
        XPath xpaPageId = XPath.newInstance("./@pageid");
        XPath xpaNs = XPath.newInstance("./@ns");
        XPath xpaTitle = XPath.newInstance("./@title");
        while (iter.hasNext()) {
          Element currentNode = (Element) iter.next();
          Page link = DataManager.getPage(
              page.getWikipedia(), xpaTitle.valueOf(currentNode), null, null);
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
   * @param wikipedia Wikipedia.
   * @param page The page.
   */
  public void retrieveTemplates(EnumWikipedia wikipedia, Page page)
      throws APIException {
    List<Page> templates = new ArrayList<Page>();
    List<Page> newTemplates = new ArrayList<Page>();
    newTemplates.add(page);
    StringBuilder titles = new StringBuilder();
    do {
      Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
      titles.setLength(0);
      int count = 0;
      while ((count < (MAX_PAGES_PER_QUERY / 5)) && !newTemplates.isEmpty()) {
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
            getRoot(wikipedia, properties, MAX_ATTEMPTS),
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
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(EnumWikipedia wikipedia, List<Page> pages) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("redirects", "");
    StringBuilder titles = new StringBuilder();
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
            getRoot(wikipedia, properties, MAX_ATTEMPTS),
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
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeDisambiguationStatus(EnumWikipedia wikipedia, List<Page> pages)
      throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("prop", "templates");
    properties.put("tllimit", "max");
    List<Page> tmpPages = new ArrayList<Page>();
    for (int i = 0; i < pages.size(); i++) {
      Iterator<Page> iter = pages.get(i).getRedirectIteratorWithPage();
      while (iter.hasNext()) {
        Page page = iter.next();
        if (page.isInMainNamespace()) {
          if (!tmpPages.contains(page)) {
            tmpPages.add(page);
          }
        } else {
          page.setDisambiguationPage(Boolean.FALSE);
        }
      }
    }
    StringBuilder titles = new StringBuilder();
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
          Element root = getRoot(wikipedia, properties, MAX_ATTEMPTS);
          updateDisambiguationStatus(
              wikipedia, tmpPages, root,
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
   * Load site informations.
   * 
   * @param wikipedia Wikipedia.
   * @throws APIException
   */
  private void loadSiteInfo(EnumWikipedia wikipedia) throws APIException {
    Map<String, String> properties = getProperties(ACTION_API_QUERY, true);
    properties.put("meta", "siteinfo");
    properties.put("siprop", "namespaces|namespacealiases|languages|interwikimap|magicwords");
    try {
      constructSiteInfo(
          getRoot(wikipedia, properties, MAX_ATTEMPTS),
          "/api/query", wikipedia);
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
        } else if ("NeedToken".equalsIgnoreCase(result)) {
          XPath xpaToken = XPath.newInstance("./@token");
          return LoginResult.createNeedTokenLogin(xpaToken.valueOf(node));
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
          XPath xpaPageId = XPath.newInstance("./@pageid");
          Integer pageId = null;
          try {
            pageId = Integer.valueOf(xpaPageId.valueOf(node));
          } catch (NumberFormatException e) {
            //
          }
          XPath xpaPageTitle = XPath.newInstance("./@title");
          XPath xpaPageOldRevId = XPath.newInstance("./@oldrevid");
          Integer pageOldRevId = null;
          try {
            pageOldRevId = Integer.valueOf(xpaPageOldRevId.valueOf(node));
          } catch (NumberFormatException e) {
            //
          }
          XPath xpaPageNewRevId = XPath.newInstance("./@newrevid");
          Integer pageNewRevId = null;
          try {
            pageNewRevId = Integer.valueOf(xpaPageNewRevId.valueOf(node));
          } catch (NumberFormatException e) {
            //
          }
          return QueryResult.createCorrectQuery(
              pageId, xpaPageTitle.valueOf(node),
              pageOldRevId, pageNewRevId);
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
   * @param wikipedia Wikipedia.
   * @return List of namespaces.
   * @throws APIException
   */
  private void constructSiteInfo(
      Element root, String query,
      EnumWikipedia wikipedia)
      throws APIException {

    // Retrieve namespaces
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

    // Retrieve namespaces aliases
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

    // Update namespace list
    LinkedList<Namespace> list = new LinkedList<Namespace>(namespaces.values());
    wikipedia.setNamespaces(list);

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
      wikipedia.setLanguages(languages);
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
      wikipedia.setInterwikis(interwikis);
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
      wikipedia.setMagicWords(magicWords);
    } catch (JDOMException e) {
      log.error("Error magic words", e);
      throw new APIException("Error parsing XML result", e);
    }
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
    List<Page> links = null;
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
            page.getWikipedia(), xpaTitle.valueOf(currentNode), null, null);
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
   * @param query XPath query to retrieve the links.
   * @param knownPages Already known pages.
   * @param redirects List of redirects filled by the method.
   * @param Flag indicating if links of the page should be kept.
   * @throws APIException
   */
  private void constructLinksWithRedirects(
      Page page, Element root, String query,
      List<Page> knownPages,
      List<Page> redirects,
      boolean keepExistingLinks)
      throws APIException {
    if (page == null) {
      throw new APIException("Page is null");
    }
    List<Page> links = null;
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
            xpaRevisionId.valueOf(currentNode),
            knownPages);
        link.setNamespace(xpaNs.valueOf(currentNode));
        if (currentNode.getAttribute("pageid") != null) {
          link.setExisting(Boolean.TRUE);
        } else if (currentNode.getAttribute("missing") != null) {
          link.setExisting(Boolean.FALSE);
        }
        if ((currentNode.getAttribute("redirect") != null) && (redirects != null)) {
          // If the link is not already a redirect, add it to the list for more processing
          if (!link.isRedirect()) {
            redirects.add(link);
          }
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
      List<Page> newTemplates, List<Page> templates,
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
            Page template = DataManager.getPage(wikipedia, title, null, null);
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
        if (node.getAttribute("missing") != null) {
          page.setExisting(Boolean.FALSE);
        }
        XPath xpaEditToken = XPath.newInstance("./@edittoken");
        page.setEditToken(xpaEditToken.valueOf(node));
        XPath xpaPageId = XPath.newInstance("./@pageid");
        page.setPageId(xpaPageId.valueOf(node));
        XPath xpaStartTimestamp = XPath.newInstance("./@starttimestamp");
        page.setStartTimestamp(xpaStartTimestamp.valueOf(node));
      }
      XPath xpa = XPath.newInstance(query + "/revisions/rev");
      node = (Element) xpa.selectSingleNode(root);
      if (node != null) {
        XPath xpaContents = XPath.newInstance(".");
        XPath xpaRevision = XPath.newInstance("./@revid");
        XPath xpaTimestamp = XPath.newInstance("./@timestamp");
        page.setContents(xpaContents.valueOf(node));
        page.setExisting(Boolean.TRUE);
        page.setRevisionId(xpaRevision.valueOf(node));
        page.setContentsTimestamp(xpaTimestamp.valueOf(node));
      }
      xpa = XPath.newInstance(query + "/protection/pr[@type=\"edit\"]");
      node = (Element) xpa.selectSingleNode(root);
      if (node != null) {
        XPath xpaLevel = XPath.newInstance("./@level");
        page.setEditProtectionLevel(xpaLevel.valueOf(node));
      }
    } catch (JDOMException e) {
      log.error("Error contents for page " + page.getTitle(), e);
      throw new APIException("Error parsing XML result", e);
    }
    return redirect;
  }

  /**
   * @param pages Pages.
   * @param root Root element.
   * @param query XPath query to retrieve the contents 
   * @throws APIException
   */
  private void constructContents(List<Page> pages, Element root, String query)
      throws APIException {
    if (pages == null) {
      throw new APIException("Pages is null");
    }
    try {
      XPath xpaPage = XPath.newInstance(query);
      XPath xpaTitle = XPath.newInstance("./@title");
      XPath xpaRev = XPath.newInstance("./revisions/rev");
      XPath xpaContents = XPath.newInstance(".");
      List resultPages = xpaPage.selectNodes(root);
      Iterator iterPages = resultPages.iterator();
      while (iterPages.hasNext()) {
        Element currentPage = (Element) iterPages.next();
        String title = xpaTitle.valueOf(currentPage);
        Element currentRev = (Element) xpaRev.selectSingleNode(currentPage);
        String contents = xpaContents.valueOf(currentRev);
        
        for (Page page : pages) {
          if (Page.areSameTitle(page.getTitle(), title)) {
            page.setContents(contents);
          }
        }
      }
    } catch (JDOMException e) {
      log.error("Error contents for pages", e);
      throw new APIException("Error parsing XML result", e);
    }
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
      List<Page> pages,
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
                    p.getWikipedia(), xpaTitle.valueOf(to), null, null);
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
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @param root Root element.
   * @param query XPath query to retrieve the list of templates.
   * @throws APIException
   */
  private void updateDisambiguationStatus(
      EnumWikipedia wikipedia,
      List<Page> pages,
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
              if (wikipedia.isDisambiguationTemplate(templateName, this)) {
                disambiguation = Boolean.TRUE;
              }
              if (wikipedia.isWiktionaryTemplate(templateName)) {
                wiktionaryLink = Boolean.TRUE;
              }
            }
            if ((p.isDisambiguationPage() == null) ||
                (Boolean.TRUE.equals(disambiguation))) {
              p.setDisambiguationPage(disambiguation);
            }
            if ((p.hasWiktionaryLink() == null) ||
                (Boolean.TRUE.equals(wiktionaryLink))) {
              p.setWiktionaryLink(wiktionaryLink);
            }
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
  private Map<String, String> getProperties(
      String action,
      boolean newApi) {
    Map<String, String> properties = new HashMap<String, String>();
    properties.put(newApi ? "action" : "what", action);
    properties.put("format", "xml");
    return properties;
  }

  /**
   * Returns the root element of the XML document returned by MediaWiki API.
   * 
   * @param wikipedia Wikipedia.
   * @param properties Properties to drive the API.
   * @param maxTry Maximum number of tries.
   * @return Root element.
   * @throws APIException
   */
  private Element getRoot(
      EnumWikipedia       wikipedia,
      Map<String, String> properties,
      int                 maxTry)
      throws JDOMParseException, APIException {
    Element root = null;
    PostMethod method = null;
    int attempt = 0;
    do {
      attempt++;
      try {
        try {
          String url = wikipedia.getApiURL();
          StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder(url) : null;
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
              if (DEBUG_URL) {
                int start = 0;
                while ((start < value.length()) && Character.isWhitespace(value.charAt(start))) {
                  start++;
                }
                if (value.indexOf('\n', start) > 0) {
                  value = value.substring(start, value.indexOf('\n', start)) + "...";
                }
                debugUrl.append(
                    (first ? "?" : "&") +
                    key + "=" +
                    ("lgpassword".equals(key) ? "XXXXX" : value));
                first = false;
              }
            }
            if (DEBUG_URL) {
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
   * @throws APIException
   */
  private void checkForError(Element root) throws APIException {
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
          String text = "Error reported: " + xpaCode.valueOf(currentNode) + " - " + xpaInfo.valueOf(currentNode);
          log.warn(text);
          throw new APIException(text, xpaCode.valueOf(currentNode));
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
    if ((value != null) && (value.indexOf("\"") != -1)) {
      return XPath.newInstance(element + "[@" + attribute + "=\"" + xmlOutputter.escapeAttributeEntities(value) + "\"]");
    }
    return XPath.newInstance(element + "[@" + attribute + "=\"" + value + "\"]");
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
