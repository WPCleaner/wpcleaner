/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for automatic Check Wiki fixing.
 */
public class AutomaticCWWorker extends AutomaticFixWorker {

  /** Maximum number of pages. */
  private final int max;

  /** True to ignore limit for non Labs errors. */
  private final boolean noLimit;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param selectedAlgorithms List of selected algorithms.
   * @param max Maximum number of pages for each algorithm.
   * @param noLimit Ignore limit for non Labs errors.
   * @param allAlgorithms List of possible algorithms.
   * @param extraComment Extra comment.
   * @param saveModifications True if modifications should be saved.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      int max, boolean noLimit,
      List<CheckErrorAlgorithm> allAlgorithms,
      String extraComment,
      boolean saveModifications, boolean analyzeNonFixed) {
    super(
        wiki, window,
        selectedAlgorithms, allAlgorithms, null,
        extraComment, saveModifications, true, analyzeNonFixed);
    this.max = max;
    this.noLimit = noLimit;
  }

  /** 
   * Compute the value to be returned by the <code>get</code> method. 
   * 
   * @return Object returned by the <code>get</code> method.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    try {
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        if (!shouldContinue()) {
          return null;
        }
        analyzeAlgorithm(algorithm);
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * Analyze an algorithm.
   * 
   * @param algorithm Algorithm.
   * @throws APIException
   */
  private void analyzeAlgorithm(CheckErrorAlgorithm algorithm) throws APIException {

    // Check if analysis is useful
    if (!saveModifications) {
      if (algorithm.getErrorNumber() >= CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST) {
        return;
      }
    }

    // Configuration
    setText(
        GT._T("Checking for errors n°{0}", Integer.toString(algorithm.getErrorNumber())) +
        " - " + algorithm.getShortDescriptionReplaced());
    int maxSize = max;
    if (noLimit && algorithm.hasSpecialList()) {
      maxSize = Integer.MAX_VALUE;
    }

    // Analysis
    List<AlgorithmError> errors = new ArrayList<>();
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    checkWiki.retrievePages(algorithm, maxSize, getWikipedia(), errors);
    List<CheckErrorAlgorithm> algorithms = selectedAlgorithms;
    while (!errors.isEmpty() && shouldContinue()) {
      AlgorithmError error = errors.remove(0);
      int maxErrors = error.getPageCount();
      for (int numPage = 0;
          (error.getPageCount() > 0) && shouldContinue();
          numPage++) {
        Page page = error.getPage(0);
        error.remove(page);
        analyzePage(
            page, algorithms,
            algorithm.getErrorNumberString() + " - " + (numPage + 1) + "/" + maxErrors);
      }
    }
  }
}
