/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfiguration;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;


/**
 * Features for Check Wiki project.
 */
public class CheckWiki {

  /**
   * True if <tt>checkwiki_bots.cgi</tt> should be used.
   */
  private final static boolean useBotList = true;

  /**
   * Access to Tool Server.
   */
  private final HttpServer toolServer;

  /**
   * Access to WMF labs.
   */
  private final HttpServer labs;

  /**
   * @param toolServer Tool Server
   */
  CheckWiki(HttpServer toolServer, HttpServer labs) {
    this.toolServer = toolServer;
    this.labs = labs;
  }

  /**
   * Retrieve list of pages for a given error.
   * 
   * @param algorithm Algorithm.
   * @param errorLimit Maximum number of pages.
   * @param wiki Wiki.
   * @param errors List of errors.
   * @throws APIException
   */
  public void retrievePages(
      final CheckErrorAlgorithm algorithm, int errorLimit,
      final EnumWikipedia wiki,
      final List<CheckError> errors) throws APIException {
    if (algorithm.getErrorNumber() > CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
      return;
    }

    // Retrieving list of pages for the error number
    boolean useLabs = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_USE_LABS);
    String code = wiki.getSettings().getCodeCheckWiki().replace("-", "_");
    Map<String, String> properties = new HashMap<String, String>();
    properties.put("id", algorithm.getErrorNumberString());
    properties.put("limit", Integer.toString(errorLimit));
    properties.put("offset", Integer.toString(0));
    properties.put("project", code);
    if (!useBotList) {
      properties.put("view", "bots");
      String url = useLabs ?
          "checkwiki/cgi-bin/checkwiki.cgi" :
          "~sk/cgi-bin/checkwiki/checkwiki.cgi";
      HttpServer server = useLabs ? labs : toolServer;
      server.sendPost(
          url, properties,
          new PagesResponseManager(true, algorithm, wiki, errors));
    } else {
      properties.put("action", "list");
      String url = useLabs ?
          "checkwiki/cgi-bin/checkwiki_bots.cgi" :
          "~sk/cgi-bin/checkwiki/checkwiki_bots.cgi";
      HttpServer server = useLabs ? labs : toolServer;
      server.sendPost(
          url, properties,
          new PagesResponseManager(false, algorithm, wiki, errors));
    }
  }

  /**
   * @param wiki Wiki.
   * @param algorithm Algorithm.
   * @return URL for algorithm description.
   */
  public String getUrlDescription(
      final EnumWikipedia wiki,
      final CheckErrorAlgorithm algorithm) {
    boolean useLabs = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_USE_LABS);
    String path = useLabs ?
        "checkwiki/cgi-bin/checkwiki.cgi" :
        "~sk/cgi-bin/checkwiki/checkwiki.cgi";
    HttpServer server = useLabs ? labs : toolServer;
    String url =
        server.getBaseUrl() + path +
        "?id=" + algorithm.getErrorNumberString() +
        "&project=" + wiki.getSettings().getCodeCheckWiki() +
        "&view=only";
    return url;
  }

  /**
   * Response manager for the list of pages.
   */
  private static class PagesResponseManager implements ResponseManager {

    /**
     * True if classic interface should be used.
     */
    private final boolean classic;

    /**
     * Algorithm.
     */
    private final Integer errorNumber;

    /**
     * Wiki.
     */
    private final EnumWikipedia wiki;

    /**
     * List of errors.
     */
    private final List<CheckError> errors;

    /**
     * @param classic True if classic interface should be used.
     */
    public PagesResponseManager(
        boolean classic,
        CheckErrorAlgorithm algorithm,
        EnumWikipedia wiki,
        List<CheckError> errors) {
      this.classic = classic;
      Integer tmp = null;
      try {
        tmp = Integer.valueOf(algorithm.getErrorNumberString());
      } catch (NumberFormatException e) {
        //
      }
      this.errorNumber = tmp;
      this.wiki = wiki;
      this.errors = errors;
    }

    /**
     * @param stream
     * @throws IOException
     * @throws APIException
     * @see org.wikipediacleaner.api.ResponseManager#manageResponse(java.io.InputStream)
     */
    public void manageResponse(InputStream stream) throws IOException,
        APIException {
      if (classic) {
        CheckError.addCheckErrorClassic(
            errors, wiki,
            errorNumber, stream);
      } else {
        CheckError.addCheckErrorBots(
            errors, wiki,
            errorNumber, stream);
      }
    }
    
  }

  /**
   * Mark a page as fixed.
   * 
   * @param page Page.
   * @param errorNumber Error number.
   * @return True if it has been done.
   */
  public boolean markAsFixed(Page page, String errorNumber) {
    try {
      int error = Integer.parseInt(errorNumber);
      if (error > CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
        return true;
      }
      EnumWikipedia wiki = page.getWikipedia();
      boolean useLabs = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_USE_LABS);
      boolean markBoth = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_MARK_BOTH);
      String code = wiki.getSettings().getCodeCheckWiki().replace("-", "_");
      Map<String, String> properties = new HashMap<String, String>();
      properties.put("id", Integer.toString(error));
      properties.put("project", code);

      // Tool Server
      if ((!useLabs || markBoth) && (page.getPageId() != null)) {
        properties.put("pageid", Integer.toString(page.getPageId()));
        if (!useBotList) {
          properties.put("view", "only");
          toolServer.sendPost("~sk/cgi-bin/checkwiki/checkwiki.cgi", properties, null);
        } else {
          properties.put("action", "mark");
          toolServer.sendPost("~sk/cgi-bin/checkwiki/checkwiki_bots.cgi", properties, null);
        }
      }

      // WMF Labs
      if (useLabs || markBoth) {
        properties.put("title", page.getTitle());
        if (!useBotList) {
          properties.put("view", "only");
          labs.sendPost("checkwiki/cgi-bin/checkwiki.cgi", properties, null);
        } else {
          properties.put("action", "mark");
          labs.sendPost("checkwiki/cgi-bin/checkwiki_bots.cgi", properties, null);
        }
      }
    } catch (NumberFormatException e) {
      return false;
    } catch (APIException e) {
      return false;
    }
    return true;
  }

  /**
   * Retrieve configuration for Check Wiki project.
   * 
   * @param wiki Wiki.
   * @param listener Listener for messages.
   * @throws APIException
   */
  public void retrieveConfiguration(
      EnumWikipedia wiki,
      MediaWikiListener listener) throws APIException {
    // Retrieve general configuration
    final CWConfiguration cwConfiguration = wiki.getCWConfiguration();
    String code = wiki.getSettings().getCodeCheckWiki().replace("-", "_");
    try {
      ResponseManager manager = new ResponseManager() {
        
        public void manageResponse(InputStream stream) throws IOException, APIException {
          if (stream != null) {
            cwConfiguration.setGeneralConfiguration(
                new InputStreamReader(stream, "UTF-8"));
          }
        }
      };
      toolServer.sendGet(
          "~sk/checkwiki/" + code + "/" + code + "_translation.txt",
          manager);
    } catch (APIException e) {
      System.err.println("Error retrieving Check Wiki configuration: " + e.getMessage());
    }

    // Retrieve specific configuration
    final WPCConfiguration wpcConfiguration = wiki.getConfiguration();
    try {
      String translationPage = wpcConfiguration.getString(WPCConfigurationString.CW_TRANSLATION_PAGE);
      if (translationPage != null) {
        MediaWiki mw = MediaWiki.getMediaWikiAccess(listener);
        Page page = DataManager.getPage(
            wiki, translationPage,
            null, null, null);
        mw.retrieveContents(wiki, page, true, false, false, false, false);
        if (Boolean.TRUE.equals(page.isExisting())) {
          cwConfiguration.setWikiConfiguration(new StringReader(page.getContents()));
        }
      }
    } catch (APIException e) {
      System.err.println("Error retrieving Check Wiki configuration: " + e.getMessage());
    }

    // Retrieving white lists
    HashMap<String, Page> whiteListPages = new HashMap<String, Page>();
    for (int i = 0; i < CWConfiguration.MAX_ERROR_NUMBER; i++) {
      CWConfigurationError error = cwConfiguration.getErrorConfiguration(i);
      if ((error != null) && (error.getWhiteListPageName() != null)) {
        Page page = DataManager.getPage(
            wiki, error.getWhiteListPageName(), null, null, null);
        whiteListPages.put(error.getWhiteListPageName(), page);
      }
    }
    if (whiteListPages.size() > 0) {
      APIFactory.getAPI().retrieveLinks(wiki, whiteListPages.values());
      for (int i = 0; i < CWConfiguration.MAX_ERROR_NUMBER; i++) {
        CWConfigurationError error = cwConfiguration.getErrorConfiguration(i);
        if ((error != null) && (error.getWhiteListPageName() != null)) {
          Page page = whiteListPages.get(error.getWhiteListPageName());
          error.setWhiteList(page);
        }
      }
    }
    CheckErrorAlgorithms.initializeAlgorithms(wiki);
  }
}
