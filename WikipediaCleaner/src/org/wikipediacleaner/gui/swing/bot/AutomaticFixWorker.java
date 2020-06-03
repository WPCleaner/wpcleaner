/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.bot;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.AutomaticFormatter;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for automatic fixing.
 */
public abstract class AutomaticFixWorker extends BasicWorker {

  /** Algorithms for which to fix pages. */
  protected final List<CheckErrorAlgorithm> selectedAlgorithms;

  /** List of potential algorithms to fix. */
  private final List<CheckErrorAlgorithm> allAlgorithms;

  /** Namespaces in which to fix pages */
  protected final Set<Integer> selectedNamespaces;

  /** Extra comment. */
  private final String extraComment;

  /** True if modifications should be saved. */
  protected final boolean saveModifications;

  /** True if pages that couldn't be fixed should be analyzed. */
  private final boolean analyzeNonFixed;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param selectedAlgorithms List of selected algorithms.
   * @param allAlgorithms List of possible algorithms.
   * @param selectedNamespaces List of selected namespaces.
   * @param extraComment Extra comment.
   * @param saveModifications True if modifications should be saved.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticFixWorker(
      EnumWikipedia wiki, BasicWindow window,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      Collection<Integer> selectedNamespaces,
      String extraComment,
      boolean saveModifications, boolean analyzeNonFixed) {
    super(wiki, window);
    if (selectedAlgorithms != null) {
      this.selectedAlgorithms = Collections.unmodifiableList(selectedAlgorithms);
    } else {
      this.selectedAlgorithms = Collections.emptyList();
    }
    if (allAlgorithms != null) {
      this.allAlgorithms = Collections.unmodifiableList(allAlgorithms);
    } else {
      this.allAlgorithms = Collections.emptyList();
    }
    this.selectedNamespaces = new HashSet<>();
    if (selectedNamespaces != null) {
      this.selectedNamespaces.addAll(selectedNamespaces);
    } else {
      this.selectedNamespaces.add(Namespace.MAIN);
    }
    this.extraComment = extraComment;
    this.saveModifications = saveModifications;
    this.analyzeNonFixed = analyzeNonFixed;
    this.countModified = 0;
    this.countMarked = 0;
    this.countMarkedOther = 0;
  }

  /**
   * @param analysis Page analysis.
   * @return True if bot modifications should be prevented.
   */
  protected boolean preventBot(PageAnalysis analysis) {
    WPCConfiguration config = getWikipedia().getConfiguration();
    List<String[]> nobotTemplates = config.getStringArrayList(
        WPCConfigurationStringList.NOBOT_TEMPLATES);
    if (nobotTemplates == null) {
      return false;
    }
    for (String[] nobotTemplate : nobotTemplates) {
      String templateName = nobotTemplate[0];
      List<PageElementTemplate> templates = analysis.getTemplates(templateName);
      if ((templates != null) && (!templates.isEmpty())) {
        return true;
      }
    }
    return false;
  }

  /**
   * Analyze and fix a page.
   * 
   * @param page Page.
   * @param algorithm Main algorithm.
   * @param prefix Prefix for the message
   * @throws APIException
   */
  protected void analyzePage(
      Page page,
      List<CheckErrorAlgorithm> algorithms,
      String prefix) throws APIException {

    // Retrieve page analysis 
    PageAnalysis analysis = getPageAnalysis(page);
    if (analysis == null) {
      return;
    }

    // Analyze page to check if an error has been found
    List<CheckErrorPage> errorPages = AlgorithmError.analyzeErrors(algorithms, analysis, true);
    boolean found = false;
    if (errorPages != null) {
      for (CheckErrorPage errorPage : errorPages) {
        if (errorPage.getErrorFound()) {
          found = true;
        }
      }
    }

    // Handle depending on whether errors were found
    if (found) {
      handleFound(algorithms, page, analysis, prefix);
    } else {
      handleNotFound(algorithms, page);
    }
  }

  /**
   * @param page Page.
   * @return Page analysis.
   * @throws APIException If a problem occurred.
   */
  private PageAnalysis getPageAnalysis(Page page) throws APIException {

    if (!selectedNamespaces.contains(page.getNamespace())) {
      return null;
    }

    setText(GT._T("Analyzing page {0}", page.getTitle()));

    // Retrieve page content 
    API api = APIFactory.getAPI();
    api.retrieveContents(getWikipedia(), Collections.singletonList(page), true, false);
    return page.getAnalysis(page.getContents(), true);
  }

  /**
   * Handle when errors are found in a page.
   * 
   * @param algorithms Algorithms that were supposed to be found.
   * @param page Page.
   * @param analysis Page analysis.
   * @param prefix Optional prefix for information.
   */
  private void handleFound(
      List<CheckErrorAlgorithm> algorithms,
      Page page, PageAnalysis analysis,
      String prefix) throws APIException {

    // Handle when no modifications are found
    if (!saveModifications) {
      return;
    }

    // Fix all errors that can be fixed
    String newContents = page.getContents();
    List<AlgorithmError.Progress> errorsFixed = new ArrayList<>();
    if (!preventBot(analysis)) {
      newContents = AutomaticFormatter.tidyArticle(page, newContents, allAlgorithms, true, errorsFixed);
    }

    // Check if error has been fixed
    boolean isFixed = false;
    if (!newContents.equals(page.getContents())) {
      for (AlgorithmError.Progress errorFixed : errorsFixed) {
        if (algorithms.contains(errorFixed.algorithm)) {
          isFixed = true;
        }
      }
    }

    // Save page if errors have been fixed
    if (isFixed) {
      StringBuilder comment = new StringBuilder();
      if ((extraComment != null) && (extraComment.trim().length() > 0)) {
        comment.append(extraComment.trim());
        comment.append(" - ");
      }
      comment.append(getWikipedia().getCWConfiguration().getComment(errorsFixed));
      setText(
          ((prefix != null) ? (prefix + " - ") : "") +
          GT._T("Fixing page {0}", page.getTitle()));
      API api = APIFactory.getAPI();
      api.updatePage(
          getWikipedia(), page, newContents,
          comment.toString(),
          true, true, false);
      incrementModified();
      for (AlgorithmError.Progress errorFixed : errorsFixed) {
        CheckErrorAlgorithm usedAlgorithm = errorFixed.algorithm;
        CheckErrorPage errorPage = AlgorithmError.analyzeError(usedAlgorithm, page.getAnalysis(newContents, true));
        if ((errorPage != null) && (!errorPage.getErrorFound())) {
          CheckWiki checkWiki = APIFactory.getCheckWiki();
          checkWiki.markAsFixed(page, usedAlgorithm.getErrorNumberString());
          if (selectedAlgorithms.contains(usedAlgorithm)) {
            incrementMarked();
          } else {
            incrementMarkedOther();
          }
        }
      }
    } else if (analyzeNonFixed) {
      Controller.runFullAnalysis(page.getTitle(), null, getWikipedia());
    }
  }

  /**
   * Handle when errors are not found in a page.
   * 
   * @param algorithms Algorithms that were supposed to be found.
   * @param page Page.
   */
  private void handleNotFound(List<CheckErrorAlgorithm> algorithms, Page page) {
    if ((algorithms == null) || (page == null)) {
      return;
    }
    for (CheckErrorAlgorithm algorithm : algorithms) {
      handleNotFound(algorithm, page);
    }
  }

  /**
   * Handle when an error is not found in a page.
   * 
   * @param algorithm Algorithm that was supposed to be found.
   * @param page Page.
   */
  private void handleNotFound(CheckErrorAlgorithm algorithm, Page page) {
    if ((algorithm == null) ||
        (algorithm.getErrorNumber() >= CheckErrorAlgorithm.MAX_ERROR_NUMBER_WITH_LIST)) {
      return;
    }
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    Boolean errorDetected = checkWiki.isErrorDetected(
        page, algorithm.getErrorNumber());
    if (Boolean.FALSE.equals(errorDetected)) {
      checkWiki.markAsFixed(page, algorithm.getErrorNumberString());
      incrementMarked();
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
      StringBuilder message = new StringBuilder();
      message.append(GT.__(
          "{0} page has been modified",
          "{0} pages have been modified",
          countModified, Integer.toString(countModified)));
      message.append("\n");
      message.append(GT.__(
          "{0} page has been marked as fixed for the selected algorithms",
          "{0} pages have been marked as fixed for the selected algorithms",
          countMarked, Integer.toString(countMarked)));
      message.append("\n");
      message.append(GT.__(
          "{0} page has been marked as fixed for other algorithms",
          "{0} pages have been marked as fixed for other algorithms",
          countMarkedOther, Integer.toString(countMarkedOther)));
      Utilities.displayInformationMessage(
          getWindow().getParentComponent(), message.toString());
    }
  }

  /* ======================================================================== */
  /* Statistics                                                               */
  /* ======================================================================== */

  /** Count of modified pages. */
  private int countModified;

  /** Count of marked pages. */
  private int countMarked;

  /** Count of marked pages for other algorithms. */
  private int countMarkedOther;

  /**
   * Increase count of modified pages.
   */
  protected void incrementModified() {
    countModified++;
  }

  /**
   * Increase count of marked pages.
   */
  protected void incrementMarked() {
    countMarked++;
  }

  /**
   * Increase count of marked pages for other algorithms.
   */
  protected void incrementMarkedOther() {
    countMarkedOther++;
  }
}
