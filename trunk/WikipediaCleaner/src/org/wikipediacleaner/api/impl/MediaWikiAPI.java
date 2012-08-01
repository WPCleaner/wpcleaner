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
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethod;
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
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.CaptchaException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.LoginResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.api.request.ApiCategoriesRequest;
import org.wikipediacleaner.api.request.ApiCategoriesResult;
import org.wikipediacleaner.api.request.ApiCategoryMembersRequest;
import org.wikipediacleaner.api.request.ApiCategoryMembersResult;
import org.wikipediacleaner.api.request.ApiEmbeddedInRequest;
import org.wikipediacleaner.api.request.ApiEmbeddedInResult;
import org.wikipediacleaner.api.request.ApiExpandRequest;
import org.wikipediacleaner.api.request.ApiExpandResult;
import org.wikipediacleaner.api.request.ApiLanguageLinksRequest;
import org.wikipediacleaner.api.request.ApiLanguageLinksResult;
import org.wikipediacleaner.api.request.ApiLinksRequest;
import org.wikipediacleaner.api.request.ApiLinksResult;
import org.wikipediacleaner.api.request.ApiLoginRequest;
import org.wikipediacleaner.api.request.ApiLoginResult;
import org.wikipediacleaner.api.request.ApiParseRequest;
import org.wikipediacleaner.api.request.ApiParseResult;
import org.wikipediacleaner.api.request.ApiPurgeRequest;
import org.wikipediacleaner.api.request.ApiPurgeResult;
import org.wikipediacleaner.api.request.ApiBacklinksRequest;
import org.wikipediacleaner.api.request.ApiBacklinksResult;
import org.wikipediacleaner.api.request.ApiRandomPagesRequest;
import org.wikipediacleaner.api.request.ApiRandomPagesResult;
import org.wikipediacleaner.api.request.ApiRawWatchlistRequest;
import org.wikipediacleaner.api.request.ApiRawWatchlistResult;
import org.wikipediacleaner.api.request.ApiSearchRequest;
import org.wikipediacleaner.api.request.ApiSearchResult;
import org.wikipediacleaner.api.request.ApiSiteInfoRequest;
import org.wikipediacleaner.api.request.ApiSiteInfoResult;
import org.wikipediacleaner.api.request.ApiRequest;
import org.wikipediacleaner.api.request.ApiTemplatesRequest;
import org.wikipediacleaner.api.request.ApiTemplatesResult;
import org.wikipediacleaner.api.request.ConnectionInformation;
import org.wikipediacleaner.api.request.xml.ApiXmlCategoriesResult;
import org.wikipediacleaner.api.request.xml.ApiXmlCategoryMembersResult;
import org.wikipediacleaner.api.request.xml.ApiXmlEmbeddedInResult;
import org.wikipediacleaner.api.request.xml.ApiXmlExpandResult;
import org.wikipediacleaner.api.request.xml.ApiXmlLanguageLinksResult;
import org.wikipediacleaner.api.request.xml.ApiXmlLinksResult;
import org.wikipediacleaner.api.request.xml.ApiXmlLoginResult;
import org.wikipediacleaner.api.request.xml.ApiXmlParseResult;
import org.wikipediacleaner.api.request.xml.ApiXmlPropertiesResult;
import org.wikipediacleaner.api.request.xml.ApiXmlPurgeResult;
import org.wikipediacleaner.api.request.xml.ApiXmlBacklinksResult;
import org.wikipediacleaner.api.request.xml.ApiXmlRandomPagesResult;
import org.wikipediacleaner.api.request.xml.ApiXmlRawWatchlistResult;
import org.wikipediacleaner.api.request.xml.ApiXmlSearchResult;
import org.wikipediacleaner.api.request.xml.ApiXmlSiteInfoResult;
import org.wikipediacleaner.api.request.xml.ApiXmlTemplatesResult;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * MediaWiki API implementation.
 */
public class MediaWikiAPI implements API {

  private final Log log = LogFactory.getLog(MediaWikiAPI.class);

  private final static int MAX_PAGES_PER_QUERY = 50;

  private static boolean DEBUG_TIME = false;
  private static boolean DEBUG_URL = true;
  private static boolean DEBUG_XML = false;
  private static XMLOutputter xmlOutputter = new XMLOutputter(Format.getPrettyFormat());

  private HttpClient httpClient;

  private final ConnectionInformation connection;

  /**
   * Time of last edit.
   */
  private long lastEditTime = 0;
  private final Object editLock = new Object();

  /**
   * Constructor.
   * @param manager HTTP connection manager.
   */
  public MediaWikiAPI(HttpConnectionManager manager) {
    connection = new ConnectionInformation();
    httpClient = new HttpClient(manager);
    httpClient.getParams().setParameter(
        HttpMethodParams.USER_AGENT,
        "WPCleaner (+http://en.wikipedia.org/wiki/User:NicoV/Wikipedia_Cleaner/Documentation)");
  }

  /**
   * Update configuration.
   */
  public static void updateConfiguration() {
    Configuration config = Configuration.getConfiguration();
    DEBUG_TIME = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_TIME);
    DEBUG_URL = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_URL);
    DEBUG_XML = config.getBoolean(
        null, ConfigurationValueBoolean.DEBUG_XML);
  }

  /**
   * @return Maximum number of pages per query.
   */
  public int getMaxPagesPerQuery() {
    return MAX_PAGES_PER_QUERY;
  }

  // ==========================================================================
  // User and login
  // ==========================================================================

  /**
   * Load Wiki configuration.
   * 
   * @param wiki Wiki.
   * @param userName User name.
   */
  public void loadConfiguration(
      EnumWikipedia wiki,
      String userName) throws APIException {

    // Retrieve site data
    loadSiteInfo(wiki);

    // Retrieve configuration
    if (wiki.getConfigurationPage() != null) {
      String configPageName = wiki.getConfigurationPage();
      Page page = DataManager.getPage(
          wiki, configPageName, null, null);
      retrieveContents(wiki, page, false);
      wiki.getConfiguration().setGeneralConfiguration(
          new StringReader(page.getContents()));

      if ((userName != null) && (userName.trim().length() > 0) &&
          (wiki.getUserConfigurationPage(userName) != null) &&
          (!Page.areSameTitle(wiki.getUserConfigurationPage(userName), configPageName))) {
        Page userConfigPage = DataManager.getPage(
            wiki,
            wiki.getUserConfigurationPage(userName),
            null, null);
        retrieveContents(wiki, userConfigPage, false);
        if (Boolean.TRUE.equals(userConfigPage.isExisting())) {
          wiki.getConfiguration().setUserConfiguration(
              new StringReader(userConfigPage.getContents()));
        }
      }
    }
  }

  // ==========================================================================
  // Tool server
  // ==========================================================================

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
      StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("POST " + url) : null;
      PostMethod method = new PostMethod(url);
      method.getParams().setContentCharset("UTF-8");
      method.setRequestHeader("Accept-Encoding", "gzip");
      if (parameters != null) {
        method.addParameters(parameters);
      }
      if (DEBUG_URL && (debugUrl != null)) {
        if (parameters != null) {
          for (int i = 0; i < parameters.length; i++) {
            debugUrl.append(
                (i == 0 ? "?" : "&") +
                parameters[i].getName() + "=" + parameters[i].getValue());
          }
        }
        debugText(debugUrl.toString());
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
      StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("GET  " + url) : null;
      GetMethod method = new GetMethod(url);
      method.getParams().setContentCharset("UTF-8");
      method.setRequestHeader("Accept-Encoding", "gzip");
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
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
   * Retrieves the contents of <code>page</code>.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @throws APIException
   */
  public void retrieveContents(EnumWikipedia wikipedia, Page page, boolean withRedirects)
      throws APIException {
    Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
    properties.put("inprop", "protection");
    properties.put("prop", "revisions|info");
    properties.put("titles", page.getTitle());
    properties.put("rvprop", "content|ids|timestamp");
    if (connection.getLgToken() != null) {
      properties.put("intoken", "edit");
    }
    try {
      boolean redirect = constructContents(
          page,
          getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS),
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
    Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
    properties.put("prop", "revisions|info");
    properties.put("titles", page.getTitle());
    properties.put("rvprop", "content|ids|timestamp");
    properties.put("rvsection", Integer.toString(section));
    if (connection.getLgToken() != null) {
      properties.put("intoken", "edit");
    }
    try {
      constructContents(
          page,
          getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS),
          "/api/query/pages/page");
    } catch (JDOMParseException e) {
      log.error("Error retrieving page content", e);
      throw new APIException("Error parsing XML", e);
    } catch (APIException e) {
      switch (e.getQueryResult()) {
      case RV_NO_SUCH_SECTION:
        // API Bug https://bugzilla.wikimedia.org/show_bug.cgi?id=26627
        page.setExisting(Boolean.FALSE);
        page.setContents(null);
        return;

      default:
        throw e;
      }
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void retrieveContentsWithoutRedirects(EnumWikipedia wikipedia, List<Page> pages)
      throws APIException {
    Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
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
            getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS),
            "/api/query/pages/page");
      } catch (JDOMParseException e) {
        log.error("Error retrieving redirects", e);
        throw new APIException("Error parsing XML", e);
      }
    }
  }

  /**
   * Check current time to see if edit is authorized (wait if needed).
   */
  private void checkTimeForEdit() {
    Configuration config = Configuration.getConfiguration();
    int minimumTime = config.getInt(null, ConfigurationValueInteger.TIME_BETWEEN_EDIT);
    if (minimumTime <= 0) {
      return;
    }
    synchronized (editLock) {
      long currentTime = System.currentTimeMillis();
      if (currentTime < lastEditTime + minimumTime * 1000) {
        try {
          Thread.sleep(lastEditTime + minimumTime * 1000 - currentTime);
        } catch (InterruptedException e) {
          // Nothing to do
        }
        currentTime = System.currentTimeMillis();
      }
      lastEditTime = currentTime;
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
    if (connection.getLgToken() == null) {
      throw new APIException("You must be logged in to update pages");
    }
    QueryResult result = null;
    Map<String, String> properties = getProperties(ApiRequest.ACTION_EDIT, true);
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
      checkTimeForEdit();
      boolean hasCaptcha = false;
      do {
        hasCaptcha = false;
        try {
          result = constructEdit(
              getRoot(wikipedia, properties, 1),
              "/api/edit");
        } catch (CaptchaException e) {
          String captchaAnswer = getCaptchaAnswer(wikipedia, e);
          if (captchaAnswer != null) {
            properties.put("captchaid", e.getId());
            properties.put("captchaword", captchaAnswer);
            hasCaptcha = true;
          } else {
            throw new APIException("CAPTCHA", e);
          }
        }
      } while (hasCaptcha);
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
    if (connection.getLgToken() == null) {
      throw new APIException("You must be logged in to update pages");
    }
    QueryResult result = null;
    Map<String, String> properties = getProperties(ApiRequest.ACTION_EDIT, true);
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
      checkTimeForEdit();
      boolean hasCaptcha = false;
      do {
        hasCaptcha = false;
        try {
          result = constructEdit(
              getRoot(wikipedia, properties, 1),
              "/api/edit");
        } catch (CaptchaException e) {
          String captchaAnswer = getCaptchaAnswer(wikipedia, e);
          if (captchaAnswer != null) {
            properties.put("captchaid", e.getId());
            properties.put("captchaword", captchaAnswer);
            hasCaptcha = true;
          } else {
            throw new APIException("CAPTCHA", e);
          }
        }
      } while (hasCaptcha);
    } catch (JDOMParseException e) {
      log.error("Error updating page: " + e.getMessage());
      throw new APIException("Error parsing XML", e);
    }
    return result;
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
    Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
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
        Element root = getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS);
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
   * Retrieves the back links of <code>page</code> and initialize redirect status.
   * 
   * @param wikipedia Wikipedia.
   * @param page The page.
   */
  public void retrieveBackLinksWithRedirects(EnumWikipedia wikipedia, Page page)
      throws APIException {
    Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
    properties.put("generator", "backlinks");
    properties.put("prop", "info");
    properties.put("gbltitle", page.getTitle());
    properties.put("gbllimit", "max");
    List<Page> links = new ArrayList<Page>();
    boolean blcontinue = false;
    do {
      try {
        XPath xpa = XPath.newInstance("/api/query/pages/page");
        Element root = getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS);
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
      Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
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
            getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS),
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
   * Initialize the information concerning redirects.
   * 
   * @param wikipedia Wikipedia.
   * @param pages List of pages.
   * @throws APIException
   */
  public void initializeRedirect(EnumWikipedia wikipedia, List<Page> pages) throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    Map<String, String> properties = getProperties(ApiRequest.ACTION_QUERY, true);
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
            wikipedia, pages,
            getRoot(wikipedia, properties, ApiRequest.MAX_ATTEMPTS));
      } catch (JDOMParseException e) {
        log.error("Error retrieving redirects", e);
        throw new APIException("Error parsing XML", e);
      }
    }
  }

  /**
   * @param root Root element in MediaWiki answer.
   * 
   * @param query Path to the answer.
   * @return Result of the query.
   * @throws APIException
   * @throws CaptchaException Captcha.
   */
  private QueryResult constructEdit(Element root, String query)
      throws APIException, CaptchaException {
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
        } else if ("Failure".equalsIgnoreCase(result)) {
          XPath xpaCaptcha = XPath.newInstance("./captcha");
          Element captcha = (Element) xpaCaptcha.selectSingleNode(node);
          if (captcha != null) {
            XPath xpaType = XPath.newInstance("./@type");
            CaptchaException exception = new CaptchaException("Captcha", xpaType.valueOf(captcha));
            XPath xpaMime = XPath.newInstance("./@mime");
            exception.setMime(xpaMime.valueOf(captcha));
            XPath xpaId = XPath.newInstance("./@id");
            exception.setId(xpaId.valueOf(captcha));
            XPath xpaUrl = XPath.newInstance("./@url");
            exception.setURL(xpaUrl.valueOf(captcha));
            throw exception;
          }
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
          page.isRedirect(true);
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
   * @param wiki Wiki.
   * @param pages List of pages.
   * @param root Root element.
   * @throws APIException
   */
  private void updateRedirectStatus(
      EnumWikipedia wiki,
      List<Page> pages,
      Element root)
      throws APIException {
    try {
      ApiXmlPropertiesResult result = new ApiXmlPropertiesResult(wiki, httpClient, connection);
      result.updateRedirect(root, pages);
    } catch (JDOMException e) {
      log.error("Error redirects", e);
      throw new APIException("Error parsing XML result", e);
    }
  }

  // ==========================================================================
  // API : Authentication
  // ==========================================================================

  /**
   * Login into Wiki.
   * (<code>action=login</code>).
   * 
   * @param wiki Wiki.
   * @param username User name.
   * @param password Password.
   * @param login Flag indicating if login should be done.
   * @return Login status.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Login">API:Login</a>
   */
  public LoginResult login(
      EnumWikipedia wiki,
      String username,
      String password,
      boolean login) throws APIException {
    logout();
    ApiLoginResult result = new ApiXmlLoginResult(wiki, httpClient, connection);
    ApiLoginRequest request = new ApiLoginRequest(result);
    if (login) {
      return request.login(username, password);
    }
    return LoginResult.createCorrectLogin();
  }

  /**
   * Logout.
   * (<code>action=logout</code>).
   * 
   * @see <a href="http://www.mediawiki.org/wiki/API:Logout">API:Logout</a>
   */
  public void logout() {
    connection.clean();
  }

  // ==========================================================================
  // API : Queries / Meta information
  // ==========================================================================

  /**
   * Load site information.
   * (<code>action=query</code>, <code>meta=siteinfo</code>).
   * 
   * @param wiki Wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Meta#siteinfo_.2F_si">API:Meta</a>
   */
  private void loadSiteInfo(EnumWikipedia wiki) throws APIException {
    ApiSiteInfoResult result = new ApiXmlSiteInfoResult(wiki, httpClient, connection);
    ApiSiteInfoRequest request = new ApiSiteInfoRequest(result);
    request.loadSiteInformation(true, true, true, true, true);
  }

  // ==========================================================================
  // API : Queries / Properties
  // ==========================================================================

  /**
   * Initialize the disambiguation flags of a list of <code>pages</code>.
   * (<code>action=query</code>, <code>prop=categories</code>) or
   * (<code>action=query</code>, <code>prop=templates</code>).
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#categories_.2F_cl">API:Properties#categories</a>
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#templates_.2F_tl">API:Properties#templates</a>
   */
  public void initializeDisambiguationStatus(EnumWikipedia wiki, List<Page> pages)
      throws APIException {
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }
    if (wiki.isDisambiguationPagesLoaded()) {
      for (Page page : pages) {
        page.setDisambiguationPage(wiki.isDisambiguationPage(page));
        if (page.isRedirect()) {
          for (Page page2 : page.getRedirects()) {
            page2.setDisambiguationPage(wiki.isDisambiguationPage(page2));
          }
        }
      }
    } else {
      List<Page> dabCategories = wiki.getConfiguration().getDisambiguationCategories();
      if ((dabCategories != null) && (dabCategories.size() > 0)) {
        ApiCategoriesResult result = new ApiXmlCategoriesResult(wiki, httpClient, connection);
        ApiCategoriesRequest request = new ApiCategoriesRequest(result);
        request.setDisambiguationStatus(pages);
      } else {
        ApiTemplatesResult result = new ApiXmlTemplatesResult(wiki, httpClient, connection);
        ApiTemplatesRequest request = new ApiTemplatesRequest(result);
        request.setDisambiguationStatus(pages);
      }
    }
  }

  /**
   * Retrieves the links of <code>page</code>.
   * (<code>action=query</code>, <code>prop=links</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#links_.2F_pl">API:Properties#links</a>
   */
  public void retrieveLinks(EnumWikipedia wiki, Page page)
      throws APIException {
    ApiLinksResult result = new ApiXmlLinksResult(wiki, httpClient, connection);
    ApiLinksRequest request = new ApiLinksRequest(result);
    request.loadLinks(page);
  }

  /**
   * Retrieve a specific language link in a page.
   * (<code>action=query</code>, <code>prop=langlinks</code>).
   * 
   * @param from Wiki in which the article is.
   * @param to Wiki to which the link is searched.
   * @param title Page title.
   * @return Page title in the destination wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Properties#langlinks_.2F_ll">API:Properties#langlinks</a>
   */
  public String getLanguageLink(EnumWikipedia from, EnumWikipedia to, String title)
      throws APIException {
    ApiLanguageLinksResult result = new ApiXmlLanguageLinksResult(from, httpClient, connection);
    ApiLanguageLinksRequest request = new ApiLanguageLinksRequest(result);
    return request.getLanguageLink(DataManager.getPage(from, title, null, null), to);
  }

  // ==========================================================================
  // API : Queries / Lists
  // ==========================================================================

  /**
   * Retrieves the back links of <code>page</code>.
   * (<code>action=query</code>, <code>list=backlinks</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Backlinks">API:Backlinks</a>
   */
  public void retrieveBackLinks(EnumWikipedia wiki, Page page)
      throws APIException {
    ApiBacklinksResult result = new ApiXmlBacklinksResult(wiki, httpClient, connection);
    ApiBacklinksRequest request = new ApiBacklinksRequest(result);
    request.loadBacklinks(page);
  }

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * (<code>action=query</code>, <code>list=categorymembers</code>).
   * 
   * @param wiki Wiki.
   * @param category Category.
   * @param depth Depth of lookup for sub-categories.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Categorymembers">API:Categorymembers</a>
   */
  public List<Page> retrieveCategoryMembers(
      EnumWikipedia wiki, String category, int depth) throws APIException {
    ApiCategoryMembersResult result = new ApiXmlCategoryMembersResult(wiki, httpClient, connection);
    ApiCategoryMembersRequest request = new ApiCategoryMembersRequest(result);
    return request.loadCategoryMembers(category, depth);
  }

  /**
   * Retrieves the pages in which <code>page</code> is embedded.
   * (<code>action=query</code>, <code>list=embeddedin</code>).
   * 
   * @param wiki Wiki.
   * @param page Page.
   * @param namespaces Limit to some name spaces.
   * @return List of pages where <code>page</code> is embedded.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Embeddedin">API:Embeddedin</a>
   */
  public List<Page> retrieveEmbeddedIn(
      EnumWikipedia wiki, Page page, List<Integer> namespaces) throws APIException {
    ApiEmbeddedInResult result = new ApiXmlEmbeddedInResult(wiki, httpClient, connection);
    ApiEmbeddedInRequest request = new ApiEmbeddedInRequest(result);
    return request.loadEmbeddedIn(page, namespaces);
  }

  /**
   * Retrieves random pages.
   * (<code>action=query</code>, <code>list=random</code>).
   * 
   * @param wiki Wiki.
   * @param count Number of random pages.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Random">API:Random</a>
   */
  public List<Page> getRandomPages(
      EnumWikipedia wiki, int count) throws APIException {
    ApiRandomPagesResult result = new ApiXmlRandomPagesResult(wiki, httpClient, connection);
    ApiRandomPagesRequest request = new ApiRandomPagesRequest(result);
    return request.loadRandomList(count);
  }

  /**
   * Retrieves similar pages.
   * (<code>action=query</code>, <code>list=search</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Search">API:Search</a>
   */
  public void retrieveSimilarPages(EnumWikipedia wiki, Page page)
      throws APIException {
    ApiSearchResult result = new ApiXmlSearchResult(wiki, httpClient, connection);
    ApiSearchRequest request = new ApiSearchRequest(result);
    request.searchSimilarPages(page);
  }

  /**
   * Retrieve raw watch list.
   * (<code>action=query</code>, <code>list=watchlistraw</code>).
   * 
   * @param wiki Wiki.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Watchlistraw">API:Watchlistraw</a>
   */
  public List<Page> retrieveRawWatchlist(EnumWikipedia wiki) throws APIException {
    ApiRawWatchlistResult result =
        new ApiXmlRawWatchlistResult(wiki, httpClient, connection);
    ApiRawWatchlistRequest request =
        new ApiRawWatchlistRequest(result);
    return request.loadWatchlistRaw();
  }

  // ==========================================================================
  // API : Expanding templates and rendering.
  // ==========================================================================

  /**
   * Expand templates in a text.
   * (<code>action=expandtemplates</code>).
   * 
   * @param wiki Wiki.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Text with templates expanded.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Parsing_wikitext#expandtemplates">API:Parsing wikitext</a>
   */
  public String expandTemplates(EnumWikipedia wiki, String title, String text) throws APIException {
    ApiExpandResult result = new ApiXmlExpandResult(wiki, httpClient, connection);
    ApiExpandRequest request = new ApiExpandRequest(result);
    return request.expandTemplates(title, text);
  }

  /**
   * Parse text.
   * (<code>action=parse</code>).
   * 
   * @param wiki Wiki.
   * @param title The title to use (for example in {{PAGENAME}}).
   * @param text The text with templates in it.
   * @return Parsed text.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Parsing_wikitext#parse">API:Parsing wikitext</a>
   */
  public String parseText(EnumWikipedia wiki, String title, String text) throws APIException {
    ApiParseResult result = new ApiXmlParseResult(wiki, httpClient, connection);
    ApiParseRequest request = new ApiParseRequest(result);
    return request.parseText(title, text);
  }

  // ==========================================================================
  // API : Purging pages' caches.
  // ==========================================================================

  /**
   * Purge the cache of <code>page</code>.
   * (<code>action=purge</code>).
   * 
   * @param wiki Wiki.
   * @param page The page.
   * @throws APIException
   * @see <a href="http://www.mediawiki.org/wiki/API:Purge">API:Purge</a>
   */
  public void purgePageCache(EnumWikipedia wiki, Page page)
      throws APIException {
    ApiPurgeResult result = new ApiXmlPurgeResult(wiki, httpClient, connection);
    ApiPurgeRequest request = new ApiPurgeRequest(result);
    request.purgePage(page);
  }

  // ==========================================================================
  // API : Changing wiki content / Create and edit pages.
  // ==========================================================================

  // ==========================================================================
  // General methods
  // ==========================================================================

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
    HttpMethod method = null;
    int attempt = 0;
    for (;;) {
      try {
        attempt++;
        method = createHttpMethod(wikipedia, properties);
        int statusCode = httpClient.executeMethod(method);
        if (statusCode != HttpStatus.SC_OK) {
          String message = "URL access returned " + HttpStatus.getStatusText(statusCode);
          log.error(message);
          if (attempt > maxTry) {
            log.warn("Error. Maximum attempts count reached.");
            throw new APIException(message);
          }
          try {
            Thread.sleep(30000);
          } catch (InterruptedException e) {
            // Nothing
          }
        } else {
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
        }
      } catch (JDOMParseException e) {
        // NOTE: to deal with api.php login action being disabled.
        String message = "JDOMParseException: " + e.getMessage();
        log.error(message);
        if (attempt > maxTry) {
          log.warn("Error. Maximum attempts count reached.");
          throw e;
        }
        try {
          Thread.sleep(30000);
        } catch (InterruptedException e2) {
          // Nothing
        }
      } catch (JDOMException e) {
        String message = "JDOMException: " + e.getMessage();
        log.error(message);
        if (attempt > maxTry) {
          log.warn("Error. Maximum attempts count reached.");
          throw new APIException("Error parsing XML result", e);
        }
        try {
          Thread.sleep(30000);
        } catch (InterruptedException e2) {
          // Nothing
        }
      } catch (IOException e) {
        String message = "IOException: " + e.getMessage();
        log.error(message);
        if (attempt > maxTry) {
          log.warn("Error. Maximum attempts count reached.");
          throw new APIException("Error accessing MediaWiki", e);
        }
        try {
          Thread.sleep(30000);
        } catch (InterruptedException e2) {
          // Nothing
        }
      } catch (APIException e) {
        if (!e.shouldRetry() || (attempt > e.getMaxRetry())) {
          throw e;
        }
        e.waitForRetry();
      } finally {
        if (method != null) {
          method.releaseConnection();
        }
      }
      log.warn("Error. Trying again");
    }
  }

  /**
   * Create an HttpMethod.
   * 
   * @param wikipedia Wikipedia.
   * @param properties Properties to drive the API.
   * @return HttpMethod.
   */
  private HttpMethod createHttpMethod(
      EnumWikipedia       wikipedia,
      Map<String, String> properties) {
    if (canUseGetMethod(properties)) {
      return createHttpGetMethod(wikipedia, properties);
    }
    return createHttpPostMethod(wikipedia, properties);
  }

  /**
   * @param properties Properties to drive the API.
   * @return True if GET method can be used.
   */
  private boolean canUseGetMethod(Map<String, String> properties) {
    if (properties == null) {
      return false;
    }
    String action = properties.get("action");
    if (action == null) {
      return false;
    }
    // TODO: Enable this again when API bug 36839 is fixed.
    //if (ACTION_API_QUERY.equals(action)) {
    //  return true;
    //}
    return false;
  }

  /**
   * Create an HTTP POST Method.
   * 
   * @param wikipedia Wikipedia.
   * @param properties Properties to drive the API.
   * @return POST Method
   */
  private PostMethod createHttpPostMethod(
      EnumWikipedia       wikipedia,
      Map<String, String> properties) {
    String url = wikipedia.getSettings().getApiURL();
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("POST " + url) : null;
    PostMethod method = new PostMethod(url);
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
        if (DEBUG_URL && (debugUrl != null)) {
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
              (ApiLoginRequest.PROPERTY_PASSWORD.equals(key) ? "XXXXX" : value));
          first = false;
        }
      }
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
      }
    }
    if (connection.getLgToken() != null) {
      method.addParameter(
          ApiLoginRequest.PROPERTY_PASSWORD,
          connection.getLgToken());
    }
    if (connection.getLgUserName() != null) {
      method.addParameter(
          ApiLoginRequest.PROPERTY_USER_NAME,
          connection.getLgUserName());
    }
    if (connection.getLgUserId() != null) {
      method.addParameter(
          ApiLoginRequest.PROPERTY_USER_ID,
          connection.getLgUserId());
    }
    return method;
  }

  /**
   * Create an HTTP GET Method.
   * 
   * @param wikipedia Wikipedia.
   * @param properties Properties to drive the API.
   * @return GET Method
   */
  private GetMethod createHttpGetMethod(
      EnumWikipedia       wikipedia,
      Map<String, String> properties) {

    // Initialize GET Method
    String url = wikipedia.getSettings().getApiURL();
    GetMethod method = new GetMethod(url);
    method.getParams().setContentCharset("UTF-8");
    method.setRequestHeader("Accept-Encoding", "gzip");

    // Manager query string
    StringBuilder debugUrl = (DEBUG_URL) ? new StringBuilder("GET  " + url) : null;
    List<NameValuePair> params = new ArrayList<NameValuePair>();
    if (properties != null) {
      boolean first = true;
      Iterator<Map.Entry<String, String>> iter = properties.entrySet().iterator();
      while (iter.hasNext()) {
        Map.Entry<String, String> property = iter.next();
        String key = property.getKey();
        String value = property.getValue();
        params.add(new NameValuePair(key, value));
        if (DEBUG_URL && (debugUrl != null)) {
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
              (ApiLoginRequest.PROPERTY_PASSWORD.equals(key) ? "XXXXX" : value));
        }
        first = false;
      }
      if (DEBUG_URL && (debugUrl != null)) {
        debugText(debugUrl.toString());
      }
    }
    if (connection.getLgToken() != null) {
      params.add(new NameValuePair(
          ApiLoginRequest.PROPERTY_TOKEN,
          connection.getLgToken()));
    }
    if (connection.getLgUserName() != null) {
      params.add(new NameValuePair(
          ApiLoginRequest.PROPERTY_USER_NAME,
          connection.getLgUserName()));
    }
    if (connection.getLgUserId() != null) {
      params.add(new NameValuePair(
          ApiLoginRequest.PROPERTY_USER_ID,
          connection.getLgUserId()));
    }
    NameValuePair[] tmpParams = new NameValuePair[params.size()];
    method.setQueryString(params.toArray(tmpParams));

    return method;
  }

  /**
   * @param text Text to add to debug.
   */
  private void debugText(String text) {
    if (DEBUG_TIME) {
      System.out.println("" + System.currentTimeMillis() + ": " + text);
    } else {
      System.out.println(text);
    }
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
   * Ask for captcha answer.
   * 
   * @param wikipedia Wikipedia.
   * @param captcha Captcha.
   * @return Answer.
   */
  private String getCaptchaAnswer(
      EnumWikipedia wikipedia,
      CaptchaException captcha) {
    // TODO: Move Swing parts out of API
    if (captcha == null) {
      return null;
    }
    if ((captcha.getQuestion() != null) && (captcha.getQuestion().trim().length() > 0)) {
      return Utilities.askForValue(
          null,
          GT._("This action is protected by a CAPTCHA.\nWhat is the answer to the following question ?") + "\n" + captcha.getQuestion(),
          "", null);
    }
    if ((captcha.getURL() != null) && (captcha.getURL().trim().length() > 0)) {
      Utilities.browseURL(wikipedia.getSettings().getHostURL() + captcha.getURL());
      return Utilities.askForValue(
          null,
          GT._("This action is protected by a CAPTCHA.\nWhat is the answer to the question displayed in your browser ?"),
          "", null);
    }
    return null;
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
