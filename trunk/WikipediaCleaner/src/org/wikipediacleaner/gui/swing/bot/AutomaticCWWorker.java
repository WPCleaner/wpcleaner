/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.CheckWiki;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for automatic Check Wiki fixing.
 */
class AutomaticCWWorker extends BasicWorker {

  /**
   * Algorithms for which to fix pages.
   */
  private final List<CheckErrorAlgorithm> selectedAlgorithms;

  /**
   * Maximum number of pages.
   */
  private final int max;

  /**
   * List of potential algorithms to fix.
   */
  private final List<CheckErrorAlgorithm> allAlgorithms;

  /**
   * True if pages that couldn't be fixed should be analyzed.
   */
  private final boolean analyzeNonFixed;

  /**
   * Count of pages fixed.
   */
  private int count;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param selectedAlgorithms List of selected algorithms.
   * @param max Maximum number of pages for each algorithm.
   * @param allAlgorithms List of possible algorithms.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<CheckErrorAlgorithm> selectedAlgorithms, int max,
      List<CheckErrorAlgorithm> allAlgorithms,
      boolean analyzeNonFixed) {
    super(wiki, window);
    this.selectedAlgorithms = selectedAlgorithms;
    this.max = max;
    this.allAlgorithms = allAlgorithms;
    this.analyzeNonFixed = analyzeNonFixed;
    this.count = 0;
  }

  /** 
   * Compute the value to be returned by the <code>get</code> method. 
   * 
   * @return Object returned by the <code>get</code> method.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    List<CheckError> errors = new ArrayList<CheckError>();
    try {
      CheckWiki checkWiki = APIFactory.getCheckWiki();
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        setText(
            GT._("Checking for errors n°{0}", Integer.toString(algorithm.getErrorNumber())) +
            " - " + algorithm.getShortDescriptionReplaced());
        checkWiki.retrievePages(algorithm, max, getWikipedia(), errors);
        for (CheckError error : errors) {
          for (int numPage = 0;
              (numPage < error.getPageCount()) && shouldContinue();
              numPage++) {
            analyzePage(error.getPage(numPage), algorithm);
          }
        }
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * Analyze and fix a page.
   * 
   * @param page Page.
   * @param algorithm Main algorithm.
   * @throws APIException
   */
  private void analyzePage(
      Page page, CheckErrorAlgorithm algorithm) throws APIException {

    // Retrieve page content 
    API api = APIFactory.getAPI();
    api.retrieveContents(getWikipedia(), Collections.singletonList(page), true, false);
    PageAnalysis analysis = page.getAnalysis(page.getContents(), true);

    // Check that robots are authorized to change this page 
    WPCConfiguration config = getWikipedia().getConfiguration();
    List<String[]> nobotTemplates = config.getStringArrayList(
        WPCConfigurationStringList.NOBOT_TEMPLATES);
    if ((nobotTemplates != null) && (!nobotTemplates.isEmpty())) {
      for (String[] nobotTemplate : nobotTemplates) {
        String templateName = nobotTemplate[0];
        List<PageElementTemplate> templates = analysis.getTemplates(templateName);
        if ((templates != null) && (!templates.isEmpty())) {
          if (analyzeNonFixed) {
            Controller.runFullAnalysis(page.getTitle(), null, getWikipedia());
          }
          return;
        }
      }
    }

    // Analyze page to check if error has been found
    List<CheckErrorPage> errorPages = CheckError.analyzeErrors(allAlgorithms, analysis);
    boolean found = false;
    if (errorPages != null) {
      for (CheckErrorPage errorPage : errorPages) {
        if (algorithm.equals(errorPage.getAlgorithm()) &&
            errorPage.getErrorFound()) {
          found = true;
        }
      }
    }

    CheckWiki checkWiki = APIFactory.getCheckWiki();
    if (found) {

      // Fix all errors that can be fixed
      String newContents = page.getContents();
      List<CheckErrorAlgorithm> usedAlgorithms = new ArrayList<CheckErrorAlgorithm>();
      for (CheckErrorAlgorithm currentAlgorithm : allAlgorithms) {
        String tmpContents = newContents;
        analysis = page.getAnalysis(tmpContents, true);
        newContents = currentAlgorithm.botFix(analysis);
        if (!newContents.equals(tmpContents)) {
          usedAlgorithms.add(currentAlgorithm);
        }
      }

      // Save page if errors have been fixed
      if (!newContents.equals(page.getContents())) {
        StringBuilder comment = new StringBuilder();
        comment.append(getWikipedia().getCWConfiguration().getComment());
        for (CheckErrorAlgorithm usedAlgorithm : usedAlgorithms) {
          comment.append(" - ");
          comment.append(usedAlgorithm.getShortDescriptionReplaced());
        }
        setText(GT._("Fixing page {0}", page.getTitle()));
        api.updatePage(
            getWikipedia(), page, newContents,
            getWikipedia().createUpdatePageComment(comment.toString(), null, true),
            false);
        count++;
        for (CheckErrorAlgorithm usedAlgorithm : usedAlgorithms) {
          CheckErrorPage errorPage = CheckError.analyzeError(usedAlgorithm, page.getAnalysis(newContents, true));
          if ((errorPage != null) && (!errorPage.getErrorFound())) {
            checkWiki.markAsFixed(page, usedAlgorithm.getErrorNumberString());
          }
        }
      } else if (analyzeNonFixed) {
        Controller.runFullAnalysis(page.getTitle(), null, getWikipedia());
      }
    } else if (algorithm.isFullDetection()) {
      checkWiki.markAsFixed(page, algorithm.getErrorNumberString());
    }
  }

  /**
   * Called on the event dispatching thread (not on the worker thread)
   * after the <code>construct</code> method has returned.
   * 
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    if (getWindow() != null) {
      Utilities.displayInformationMessage(
          getWindow().getParentComponent(),
          GT.__(
              "{0} page has been fixed",
              "{0} pages have been fixed",
              count, Integer.toString(count)));
    }
  }

}
