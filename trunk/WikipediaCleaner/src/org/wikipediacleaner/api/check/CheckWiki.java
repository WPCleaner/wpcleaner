/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.HttpServer;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.ResponseManager;
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
   * List of listeners for Check Wiki events.
   */
  private final List<WeakReference<CheckWikiListener>> listeners;

  /**
   * @param toolServer Tool Server
   */
  public CheckWiki(HttpServer toolServer, HttpServer labs) {
    this.toolServer = toolServer;
    this.labs = labs;
    this.listeners = new ArrayList<WeakReference<CheckWikiListener>>();
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
   * Mark a page as fixed.
   * 
   * @param page Page.
   * @param errorNumber Error number.
   * @return True if it has been done.
   */
  public boolean markAsFixed(Page page, String errorNumber) {
    try {
      int error = Integer.parseInt(errorNumber);
      notifyPageFixed(page, error);
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
    } catch (NumberFormatException e) {
      return false;
    } catch (APIException e) {
      return false;
    }
    return true;
  }

  /**
   * Checks a page for errors.
   * 
   * @param page Page to be checked.
   * @return List of detected errors.
   */
  public List<CheckWikiDetection> check(Page page) {
    try {
      List<CheckWikiDetection> detections = null;
      EnumWikipedia wiki = page.getWikipedia();
      boolean useLabs = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_USE_LABS);
      boolean markBoth = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_MARK_BOTH);
      String code = wiki.getSettings().getCodeCheckWiki().replace("-", "_");
      Map<String, String> properties = new HashMap<String, String>();
      properties.put("project", code);
      properties.put("article", page.getTitle());
      if (useLabs || markBoth) {
        detections = new ArrayList<CheckWikiDetection>();
        labs.sendPost(
            "checkwiki/cgi-bin/checkarticle.cgi", properties,
            new CheckResponseManager(detections));
      }
      return detections;
    } catch (APIException e) {
      return null;
    }
  }

  /**
   * Check a page for errors of a given type.
   * 
   * @param page Page to be checked.
   * @param errorNumber Error number.
   * @return TRUE/FALSE depending on detection, null in case of problem.
   */
  public Boolean isErrorDetected(Page page, int errorNumber) {
    Collection<CheckWikiDetection> detections = check(page);
    if (detections == null) {
      return null;
    }
    for (CheckWikiDetection detection : detections) {
      if (detection.getErrorNumber() == errorNumber) {
        return Boolean.TRUE;
      }
    }
    return Boolean.FALSE;
  }

  /**
   * @param listener Listener to be added to the list of registered listeners.
   */
  public void addListener(CheckWikiListener listener) {
    listeners.add(new WeakReference<CheckWikiListener>(listener));
  }

  /**
   * @param listener Listener to be removed from the list of registered listeners.
   */
  public void removeListener(CheckWikiListener listener) {
    if (listener == null) {
      return;
    }
    synchronized (listeners) {
      Iterator<WeakReference<CheckWikiListener>> itListener = listeners.iterator();
      while (itListener.hasNext()) {
        WeakReference<CheckWikiListener> listenerRef = itListener.next();
        CheckWikiListener currentListener = listenerRef.get();
        if ((currentListener == null) || (currentListener == listener)) {
          itListener.remove();
        }
      }
    }
  }

  /**
   * Internal notification when a page is fixed.
   * 
   * @param page Page fixed.
   * @param errorNumber Error for which the page is fixed.
   */
  private void notifyPageFixed(Page page, int errorNumber) {
    synchronized (listeners) {
      Iterator<WeakReference<CheckWikiListener>> itListener = listeners.iterator();
      while (itListener.hasNext()) {
        WeakReference<CheckWikiListener> listenerRef = itListener.next();
        CheckWikiListener listener = listenerRef.get();
        if (listener == null) {
          itListener.remove();
        } else {
          listener.pageFixed(page, errorNumber);
        }
      }
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
   * @param wiki Wiki.
   * @return Server name.
   */
  public static String getServerName(EnumWikipedia wiki) {
    boolean useLabs = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_USE_LABS);
    return useLabs ? "WMFLabs" : "Toolserver";
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
    final API api = APIFactory.getAPI();
    final WPCConfiguration wpcConfiguration = wiki.getConfiguration();
    boolean useLabs = wiki.getConfiguration().getBoolean(WPCConfigurationBoolean.CW_USE_LABS);
    final CWConfiguration cwConfiguration = wiki.getCWConfiguration();
    if (!useLabs) {
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
    }

    // Retrieve specific configuration
    try {
      String translationPage = wpcConfiguration.getString(WPCConfigurationString.CW_TRANSLATION_PAGE);
      if (translationPage != null) {
        Page page = DataManager.getPage(
            wiki, translationPage,
            null, null, null);
        api.retrieveContents(wiki, Collections.singleton(page), false, false);
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
      api.retrieveLinks(wiki, whiteListPages.values());
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
