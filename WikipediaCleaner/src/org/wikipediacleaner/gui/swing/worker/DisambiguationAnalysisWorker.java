/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class DisambiguationAnalysisWorker extends BasicWorker {

  private final Page page;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   */
  public DisambiguationAnalysisWorker(EnumWikipedia wikipedia, BasicWindow window, Page page) {
    super(wikipedia, window);
    this.page = page;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveContents(getWikipedia(), page, false, false, false, true, false);
      mw.retrieveAllLinksToPage(getWikipedia(), page, true);
      ArrayList<Page> pageAndRedirects = new ArrayList<>();
      pageAndRedirects.add(page);
      for (Page backlink : page.getAllLinksToPage()) {
        if ((backlink != null) && (backlink.getRedirects().isRedirect())) {
          pageAndRedirects.add(backlink);
          mw.retrieveContents(getWikipedia(), backlink, false, false, false, false, false);
        }
      }
      mw.retrieveDisambiguationInformation(getWikipedia(), pageAndRedirects, null, false, false, false);
      mw.retrieveAllLinks(getWikipedia(), page, null, null, true, false);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}