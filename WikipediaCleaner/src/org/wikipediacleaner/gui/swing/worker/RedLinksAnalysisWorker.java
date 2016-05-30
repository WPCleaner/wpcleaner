/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

/**
 * SwingWorker for reloading the page. 
 */
public class RedLinksAnalysisWorker extends BasicWorker {

  private final Page page;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   */
  public RedLinksAnalysisWorker(EnumWikipedia wikipedia, BasicWindow window, Page page) {
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
      mw.retrieveAllLinksToPage(getWikipedia(), page, true);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}