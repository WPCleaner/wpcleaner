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
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * SwingWorker for automatic Check Wiki fixing on a category.
 */
public class AutomaticCategoryCWWorker extends AutomaticFixWorker {

  /** Category containing the list of pages to fix. */
  private final Page category;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param category Category containing the list of pages to fix.
   * @param selectedAlgorithms List of selected algorithms.
   * @param allAlgorithms List of possible algorithms.
   * @param selectedNamespaces List of selected namespaces.
   * @param extraComment Extra comment.
   * @param saveModifications True if modifications should be saved.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticCategoryCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      Page category,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      Collection<Integer> selectedNamespaces,
      String extraComment,
      boolean saveModifications, boolean analyzeNonFixed) {
    super(
        wiki, window,
        selectedAlgorithms, allAlgorithms, selectedNamespaces,
        extraComment, saveModifications, false, analyzeNonFixed);
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
      api.retrieveCategoryMembers(getWikipedia(), category, 0, false, Integer.MAX_VALUE);
      for (Page page : category.getRelatedPages(RelatedPages.CATEGORY_MEMBERS)) {
        if (!shouldContinue()) {
          return null;
        }
        analyzePage(page, selectedAlgorithms, null);
      }
    } catch (APIException e) {
      return e;
    }
    return null;
  }
}
