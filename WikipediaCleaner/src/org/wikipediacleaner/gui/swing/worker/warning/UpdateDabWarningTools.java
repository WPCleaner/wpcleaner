/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;


/**
 * Tools for updating disambiguation warnings.
 */
public class UpdateDabWarningTools extends UpdateWarningTools {

  private final static Logger log = LoggerFactory.getLogger(UpdateDabWarningTools.class);

  private final Map<String, Page> dabPages;
  private final Map<String, Page> nonDabPages;

  /** True if links are already available in pages. */
  private boolean linksAvailable;

  /** True if disambiguation information is already available in pages. */
  private boolean dabInformationAvailable;

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateDabWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateDabWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateDabWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, new DabWarningProcessor(wiki), createWarning, automaticEdit);
    this.dabPages = new HashMap<>();
    this.nonDabPages = new HashMap<>();
  }

  /**
   * @param available True if links are already available in pages.
   */
  public void setLinksAvailable(boolean available) {
    this.linksAvailable = available;
  }

  /**
   * @param available True if disambiguation information is already available in pages.
   */
  public void setDabInformationAvailable(boolean available) {
    this.dabInformationAvailable = available;
  }

  /**
   * Load all disambiguation pages.
   */
  public void preloadDabPages() {
    dabPages.clear();
    nonDabPages.clear();
    try {
      wiki.loadDisambiguationPages(api);
    } catch (APIException e) {
      log.error("Error preloading disambiguation pages", e);
    }
  }

  /**
   * Retrieve information in the pages to construct the warning.
   * 
   * @param pages List of pages.
   * @return True if information was retrieved.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  protected boolean retrievePageInformation(
      List<Page> pages) throws APIException {
    MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);

    // Retrieving links in each page
    if (!linksAvailable) {
      for (Page page : pages) {
        mw.retrieveAllLinks(wiki, page, Namespace.MAIN, null, false, false);
      }
      mw.block(true);
      if (shouldStop()) {
        return false;
      }
    }

    // Retrieving disambiguation information in each page
    boolean hasDisambiguationLink = false;
    if (!dabInformationAvailable) {
      if (!wiki.isDisambiguationPagesLoaded()) {
        List<Page> tmpPages = new ArrayList<>();
        for (Page page : pages) {
          for (int numLink = 0; numLink < page.getLinks().size(); numLink++) {
            Page link = page.getLinks().get(numLink);
            if (dabPages.containsKey(link.getTitle())) {
              page.getLinks().set(numLink, dabPages.get(link.getTitle()));
              hasDisambiguationLink = true;
            } else if (nonDabPages.containsKey(link.getTitle())) {
              page.getLinks().set(numLink, nonDabPages.get(link.getTitle()));
            } else {
              tmpPages.add(link);
            }
          }
        }
        if (!tmpPages.isEmpty()) {
          mw.retrieveDisambiguationInformation(wiki, tmpPages, null, false, false, true);
        }
        for (Page page : tmpPages) {
          if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
            dabPages.put(page.getTitle(), page);
            hasDisambiguationLink = true;
          } else {
            nonDabPages.put(page.getTitle(), page);
          }
        }
      } else {
        for (Page page : pages) {
          List<Page> links = page.getRedirects().getLinks();
          for (int numLink = 0; numLink < links.size(); numLink++) {
            Page link = links.get(numLink);
            if (Boolean.TRUE.equals(wiki.isDisambiguationPage(link))) {
              link.setDisambiguationPage(Boolean.TRUE);
              hasDisambiguationLink = true;
            } else {
              link.setDisambiguationPage(Boolean.FALSE);
            }
          }
        }
      }
      if (shouldStop()) {
        return false;
      }
    }

    // Retrieving page contents
    if (hasDisambiguationLink && !getContentsAvailable()) {
      List<Page> tmpPages = new ArrayList<>();
      for (Page page : pages) {
        boolean toAdd = false;
        for (Page link : page.getLinks()) {
          if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
            toAdd = true;
          }
        }
        if (toAdd) {
          tmpPages.add(page);
        }
      }
      if (!tmpPages.isEmpty()) {
        mw.retrieveContents(wiki, tmpPages, true, false, false, false);
      }
    }

    return true;
  }
}
