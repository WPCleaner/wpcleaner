/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorComparator;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for reloading the page. 
 */
public class CheckWikiProjectWorker extends BasicWorker {

  final List<CheckError> errors;
  private final List<CheckErrorAlgorithm> selectedAlgorithms;
  private final int errorLimit;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param errors Error list to complete.
   * @param selectedAlgorithms List of algorithms.
   * @param otherErrors Flag indicating if other errors should be retrieved (without list).
   * @param errorLimit Maximum number of pages that should be returned for each error.
   */
  public CheckWikiProjectWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<CheckError> errors,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      boolean otherErrors,
      int errorLimit) {
    super(wikipedia, window);
    this.errors = errors;
    if (!otherErrors) {
      this.errors.clear();
    }
    this.selectedAlgorithms = selectedAlgorithms;
    this.errorLimit = errorLimit;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {

    // Retrieving errors
    boolean errorLoaded = false;
    APIException exception = null;
    if (selectedAlgorithms != null) {
      for (final CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        try {
          
          if ((algorithm != null) &&
              (algorithm.isAvailable()) &&
              (algorithm.getPriority() != CWConfigurationError.PRIORITY_BOT_ONLY)) {
            // Retrieving list of pages for the error number
            setText(
                GT._T("Checking for errors n°{0}", Integer.toString(algorithm.getErrorNumber())) +
                " - " + algorithm.getShortDescriptionReplaced());
            APIFactory.getCheckWiki().retrievePages(algorithm, errorLimit, getWikipedia(), errors);
            errorLoaded = true;
          }
        } catch (APIException e) {
          exception = e;
        }
      }
    }
    if (!errorLoaded && (exception != null)) {
      return exception;
    }

    // Sorting errors by priority
    setText(GT._T("Sorting errors by priority"));
    Collections.sort(errors, new CheckErrorComparator());

    return null;
  }
}