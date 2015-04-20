/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2015  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateWarningTools.Stats;


/**
 * Swing worker for updating various warnings.
 */
public abstract class UpdateWarningWorker extends BasicWorker {

  /** List of pages. */
  protected final List<Page> warningPages;

  /** True if the list of pages should be used. */
  protected final boolean useList;

  /** True if the contents is already available in articles. */
  protected final boolean contentsAvailable;

  /** True if edits should be considered automatic. */
  protected final boolean automaticEdit;

  /** True if this is a simulation. */
  protected final boolean simulation;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param simulation True if this is a simulation.
   */
  public UpdateWarningWorker(
      EnumWikipedia wiki, BasicWindow window,
      boolean simulation) {
    super(wiki, window);
    this.warningPages = new ArrayList<Page>();
    this.useList = false;
    this.contentsAvailable = false;
    this.automaticEdit = true;
    this.simulation = simulation;
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param pages Pages to analyze.
   * @param contentsAvailable True if contents is already available in pages.
   * @param automaticEdit True if the edit should be considered automatic.
   */
  public UpdateWarningWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<Page> pages, boolean contentsAvailable,
      boolean automaticEdit) {
    super(wiki, window);
    this.warningPages = new ArrayList<Page>(pages);
    this.useList = true;
    this.contentsAvailable = contentsAvailable;
    this.automaticEdit = automaticEdit;
    this.simulation = false;
  }

  /**
   * Generate the list of warning pages.
   * 
   * @throws APIException
   */
  protected abstract void listWarningPages() throws APIException;

  /**
   * Retrieve pages for a given error number.
   * 
   * @param errorNumber Error number.
   * @param pages List of pages to complete.
   */
  protected void retrieveCheckWikiPages(int errorNumber, List<Page> pages) {
    CheckWiki cw = APIFactory.getCheckWiki();
    EnumWikipedia wiki = getWikipedia();
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, errorNumber);
    List<CheckError> errors = new ArrayList<CheckError>();
    try {
      cw.retrievePages(algorithm, 10000, wiki, errors);
      for (CheckError error: errors) {
        for (int pageNum = 0; pageNum < error.getPageCount(); pageNum++) {
          pages.add(error.getPage(pageNum));
        }
      }
    } catch (APIException e) {
      // Nothing
    }
  }

  /**
   * Display statistics.
   * 
   * @param stats Statistics.
   * @param startTime Start time.
   */
  protected void displayStats(
      Stats stats, long startTime) {
    if (useList) {
      return;
    }
    UpdateWarningTools.displayStats(getWindow(), stats, startTime);
  }
}
