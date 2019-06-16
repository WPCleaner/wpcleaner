/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.awt.Component;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.ModificationReport;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
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

  /** List of pages on which the automatic fixing is to be done. */
  private final Page[] pages;

  /** Replacements to be done. */
  private final Map<String, List<AutomaticFixing>> replacements;

  /** Comment to use for the replacements. */
  private final String comment;

  /** Description of the replacements done. */
  private final ModificationReport report;

  /** True if the description of the replacements should be displayed. */
  private final boolean showDescription;

  /** List of CW fixing that should be done. */
  private final Collection<CheckErrorAlgorithm> automaticCW;

  /** List of CW fixing that should be done even if no automatic replacement was done. */
  private final Collection<CheckErrorAlgorithm> forceCW;

  /** True if modifications should be saved. */
  private final boolean save;

  /** True to pause after each edit */
  private final boolean pauseAfterEachEdit;

  /** True to apply also bot fixes */
  private final boolean botFix;

  /** Parent window */
  private final Component parent;

  /**
   * @param wiki Wiki.
   * @param window Associated window.
   * @param pages List of pages on which the automatic fixing is to be done.
   * @param replacements Replacements to be done.
   * @param comment Comment to use for the replacements.
   * @param showDescription True if the description of the replacements should be displayed.
   * @param automaticCW List of CW fixing that should be done.
   * @param forceCW List of CW fixing that should be done even if no automatic replacement was done.
   * @param save True if modifications should be saved.
   * @param pauseAfterEachEdit True to pause after each edit.
   * @param botFix True to apply bot fixes.
   * @param parent Parent window.
   */
  public AutomaticFixingWorker(
      EnumWikipedia wiki, BasicWindow window,
      Page[] pages, Map<String, List<AutomaticFixing>> replacements,
      String comment, boolean showDescription,
      Collection<CheckErrorAlgorithm> automaticCW, Collection<CheckErrorAlgorithm> forceCW,
      boolean save, boolean pauseAfterEachEdit, boolean botFix,
      Component parent) {
    super(wiki, window);
    this.pages = pages;
    this.replacements = replacements;
    this.comment = comment;
    this.showDescription = showDescription;
    this.report = (showDescription ? new ModificationReport() : null);
    this.automaticCW = automaticCW;
    this.forceCW = forceCW;
    this.save = save;
    this.pauseAfterEachEdit = pauseAfterEachEdit;
    this.botFix = botFix;
    this.parent = parent;
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
      Page[] tmpPages = new Page[pages.length];
      for (int numPage = 0; numPage < pages.length; numPage++) {
        tmpPages[numPage] = DataManager.getPage(
            getWikipedia(), pages[numPage].getTitle(), pages[numPage].getPageId(), null, null);
      }
      MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
      Integer count = Integer.valueOf(mw.replaceText(
          tmpPages, replacements, getWikipedia(),
          comment, report, automaticCW, forceCW,
          save, true, true, pauseAfterEachEdit, botFix, parent));
      if (showDescription && (count > 0)) {
        InformationWindow.createInformationWindow(
            GT.__(
                "The following modifications have been done ({0} page):",
                "The following modifications have been done ({0} pages):",
                count, count.toString()),
            report.getReport(getWikipedia()), true, getWikipedia());
      }
      return count;
    } catch (APIException e) {
      return e;
    }
  }
}