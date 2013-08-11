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
  private final ToolServer toolServer;

  /**
   * @param toolServer Tool Server<
   */
  CheckWiki(ToolServer toolServer) {
    this.toolServer = toolServer;
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
  public void retrievePagesForError(
      final CheckErrorAlgorithm algorithm, int errorLimit,
      final EnumWikipedia wiki,
      final List<CheckError> errors) throws APIException {
    if (algorithm.getErrorNumber() > CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
      return;
    }

    // Retrieving list of pages for the error number
    String code = wiki.getSettings().getCodeCheckWiki().replace("-", "_");
    Map<String, String> properties = new HashMap<String, String>();
    properties.put("id", algorithm.getErrorNumberString());
    properties.put("limit", Integer.toString(errorLimit));
    properties.put("offset", Integer.toString(0));
    properties.put("project", code);
    if (!useBotList) {
      properties.put("view", "bots");
      ResponseManager manager = new ResponseManager() {
        
        public void manageResponse(InputStream stream)
            throws IOException, APIException {
          CheckError.addCheckErrorClassic(
              errors, wiki,
              Integer.valueOf(algorithm.getErrorNumberString()), stream);
        }
      };
      toolServer.sendPost(
          "~sk/cgi-bin/checkwiki/checkwiki.cgi", properties, manager);
    } else {
      properties.put("action", "list");
      ResponseManager manager = new ResponseManager() {
        
        public void manageResponse(InputStream stream)
            throws IOException, APIException {
          CheckError.addCheckErrorBots(
              errors, wiki,
              Integer.valueOf(algorithm.getErrorNumberString()), stream);
        }
      };
      toolServer.sendPost(
          "~sk/cgi-bin/checkwiki/checkwiki_bots.cgi", properties, manager);
    }
  }

  /**
   * Mark a page as fixed.
   * 
   * @param page Page.
   * @param errorNumber Error number.
   * @return True if it has been done.
   */
  public boolean markPageAsFixed(Page page, String errorNumber) {
    try {
      int error = Integer.parseInt(errorNumber);
      if (error > CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
        return true;
      }
      Map<String, String> properties = new HashMap<String, String>();
      properties.put("id", Integer.toString(error));
      properties.put("pageid", Integer.toString(page.getPageId()));
      properties.put("project", page.getWikipedia().getSettings().getCodeCheckWiki());
      if (!useBotList) {
        properties.put("view", "only");
        toolServer.sendPost(
            "~sk/cgi-bin/checkwiki/checkwiki.cgi", properties, null);
      } else {
        properties.put("action", "mark");
        toolServer.sendPost(
            "~sk/cgi-bin/checkwiki/checkwiki_bots.cgi", properties, null);
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
      APIFactory.getToolServer().sendGet(
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
        mw.retrieveContents(wiki, page, true, false, false, false);
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
