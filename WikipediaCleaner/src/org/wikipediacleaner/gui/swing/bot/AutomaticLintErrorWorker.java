/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * SwingWorker for automatic Linter fixing on a category.
 */
public class AutomaticLintErrorWorker extends AutomaticFixWorker {

  /** Linter category to fix. */
  private final LinterCategory category;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param category Linter category.
   * @param selectedAlgorithms List of selected algorithms.
   * @param allAlgorithms List of possible algorithms.
   * @param selectedNamespaces List of selected namespaces.
   * @param extraComment Extra comment.
   * @param saveModifications True if modifications should be saved.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticLintErrorWorker(
      EnumWikipedia wiki, BasicWindow window,
      LinterCategory category,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      Collection<Integer> selectedNamespaces,
      String extraComment,
      boolean saveModifications, boolean analyzeNonFixed) {
    super(
        wiki, window,
        selectedAlgorithms, allAlgorithms, selectedNamespaces,
        extraComment, saveModifications, analyzeNonFixed);
    this.category = category;
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
      API api = APIFactory.getAPI();
      for (Integer namespace : selectedNamespaces) {
        List<Page> pages = api.retrieveLinterCategory(
            getWikipedia(), category.getCategory(), namespace,
            false, false, Integer.MAX_VALUE);
        while (!pages.isEmpty()) {
          if (!shouldContinue()) {
            return null;
          }
          Page page = pages.remove(0);
          try {
            analyzePage(page, selectedAlgorithms, null);
          } catch (APIException e) {
            boolean ignoreException = false;
            EnumQueryResult result = e.getQueryResult();
            if (result != null) {
              if (EnumQueryResult.PROTECTED_PAGE.equals(result)) {
                ignoreException = true;
              }
            }
            if (!ignoreException) {
              throw e;
            }
          }
        }
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}
