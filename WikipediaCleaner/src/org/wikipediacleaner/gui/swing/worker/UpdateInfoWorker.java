/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;

public class UpdateInfoWorker extends BasicWorker {

  private final Collection<Page> pageList;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param pages Pages.
   */
  public UpdateInfoWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<Page> pages) {
    super(wikipedia, window);
    this.pageList = pages;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      mw.retrieveAllLinksToPages(getWikipedia(), pageList, true);
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}