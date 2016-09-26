/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for automatic disambiguation. 
 */
public class AutomaticFixingWorker extends BasicWorker {

  /**
   * List of pages on which the automatic fixing is to be done.
   */
  private final Page[] pages;

  /**
   * Replacements to be done.
   */
  private final Map<String, List<AutomaticFixing>> replacements;

  /**
   * Comment to use for the replacements.
   */
  private final String comment;

  /**
   * Description of the replacements done.
   */
  private final StringBuilder description;

  /**
   * True if the description of the replacements should be displayed.
   */
  private final boolean showDescription;

  /**
   * True if automatic Check Wiki fixing should be done also.
   */
  private final boolean automaticCW;

  /**
   * True if modifications should be saved.
   */
  private final boolean save;

  /**
   * @param wiki Wiki.
   * @param window Associated window.
   * @param pages List of pages on which the automatic fixing is to be done.
   * @param replacements Replacements to be done.
   * @param comment Comment to use for the replacements.
   * @param showDescription True if the description of the replacements should be displayed.
   * @param automaticCW True if automatic Check Wiki fixing should be done also.
   * @param save True if modifications should be saved.
   */
  public AutomaticFixingWorker(
      EnumWikipedia wiki, BasicWindow window,
      Page[] pages, Map<String, List<AutomaticFixing>> replacements,
      String comment, boolean showDescription,
      boolean automaticCW, boolean save) {
    super(wiki, window);
    this.pages = new Page[pages.length];
    for (int numPage = 0; numPage < pages.length; numPage++) {
      this.pages[numPage] = DataManager.getPage(
          wiki, pages[numPage].getTitle(), pages[numPage].getPageId(), null, null);
    }
    this.replacements = replacements;
    this.comment = comment;
    this.showDescription = showDescription;
    this.description = (showDescription ? new StringBuilder() : null);
    this.automaticCW = automaticCW;
    this.save = save;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  /**
   * @return Count of modified pages.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      Integer count = Integer.valueOf(mw.replaceText(
          pages, replacements, getWikipedia(),
          comment, description, automaticCW, save, true));
      if (showDescription && (count > 0)) {
        InformationWindow.createInformationWindow(
            GT.__(
                "The following modifications have been done ({0} page):",
                "The following modifications have been done ({0} pages):",
                count, count.toString()),
            description.toString(), true, getWikipedia());
      }
      return count;
    } catch (APIException e) {
      return e;
    }
  }
}