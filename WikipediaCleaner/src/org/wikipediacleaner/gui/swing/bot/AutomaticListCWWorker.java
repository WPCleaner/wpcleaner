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
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.AutomaticFormatter;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for automatic Check Wiki fixing on a list of pages.
 */
public class AutomaticListCWWorker extends BasicWorker {

  /** Page containing the list of pages to fix. */
  private final Page list;

  /** Algorithms for which to fix pages. */
  private final List<CheckErrorAlgorithm> selectedAlgorithms;

  /** List of potential algorithms to fix. */
  private final List<CheckErrorAlgorithm> allAlgorithms;

  /** Extra comment. */
  private final String extraComment;

  /** True if modifications should be saved. */
  private final boolean saveModifications;

  /** True if pages that couldn't be fixed should be analyzed. */
  private final boolean analyzeNonFixed;

  /** Count of modified pages. */
  private int countModified;

  /** Count of marked pages. */
  private int countMarked;

  /** Count of marked pages for other algorithms. */
  private int countMarkedOther;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param list Page containing the list of pages to fix.
   * @param selectedAlgorithms List of selected algorithms.
   * @param allAlgorithms List of possible algorithms.
   * @param extraComment Extra comment.
   * @param saveModifications True if modifications should be saved.
   * @param analyzeNonFixed True if pages that couldn't be fixed should be analyzed.
   */
  public AutomaticListCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      Page list,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      String extraComment,
      boolean saveModifications, boolean analyzeNonFixed) {
    super(wiki, window);
    this.list = list;
    this.selectedAlgorithms = selectedAlgorithms;
    this.allAlgorithms = allAlgorithms;
    this.extraComment = extraComment;
    this.saveModifications = saveModifications;
    this.analyzeNonFixed = analyzeNonFixed;
    this.countModified = 0;
    this.countMarked = 0;
    this.countMarkedOther = 0;
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
      api.retrieveLinks(getWikipedia(), list, null, null, false, false);
      for (Page page : list.getLinks()) {
        if (!shouldContinue()) {
          return null;
        }
        try {
          analyzePage(page);
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
    } catch (APIException e) {
      return e;
    }
    return null;
  }

  /**
   * Analyze and fix a page.
   * 
   * @param page Page.
   * @throws APIException
   */
  private void analyzePage(Page page) throws APIException {

    setText(GT._T("Analyzing page {0}", page.getTitle()));

    // Retrieve page content 
    API api = APIFactory.getAPI();
    api.retrieveContents(getWikipedia(), Collections.singletonList(page), true, false);
    PageAnalysis analysis = page.getAnalysis(page.getContents(), true);

    // Check that robots are authorized to change this page
    boolean preventBot = false;
    if (saveModifications) {
      WPCConfiguration config = getWikipedia().getConfiguration();
      List<String[]> nobotTemplates = config.getStringArrayList(
          WPCConfigurationStringList.NOBOT_TEMPLATES);
      if ((nobotTemplates != null) && (!nobotTemplates.isEmpty())) {
        for (String[] nobotTemplate : nobotTemplates) {
          String templateName = nobotTemplate[0];
          List<PageElementTemplate> templates = analysis.getTemplates(templateName);
          if ((templates != null) && (!templates.isEmpty())) {
            preventBot = true;
          }
        }
      }
    }

    // Analyze page to check if an error has been found
    List<CheckErrorPage> errorPages = CheckError.analyzeErrors(selectedAlgorithms, analysis, true);
    boolean found = false;
    if (errorPages != null) {
      for (CheckErrorPage errorPage : errorPages) {
        if (errorPage.getErrorFound()) {
          found = true;
        }
      }
    }

    CheckWiki checkWiki = APIFactory.getCheckWiki();
    if (found) {
      if (!saveModifications) {
        return;
      }

      // Fix all errors that can be fixed
      String newContents = page.getContents();
      List<CheckError.Progress> errorsFixed = new ArrayList<>();
      if (!preventBot) {
        newContents = AutomaticFormatter.tidyArticle(page, newContents, allAlgorithms, true, errorsFixed);
      }

      // Check if error has been fixed
      boolean isFixed = false;
      if (!newContents.equals(page.getContents())) {
        for (CheckError.Progress errorFixed : errorsFixed) {
          if (selectedAlgorithms.contains(errorFixed.algorithm)) {
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
        setText(GT._T("Fixing page {0}", page.getTitle()));
        api.updatePage(
            getWikipedia(), page, newContents,
            comment.toString(),
            true, true, false);
        countModified++;
        for (CheckError.Progress errorFixed : errorsFixed) {
          CheckErrorAlgorithm usedAlgorithm = errorFixed.algorithm;
          CheckErrorPage errorPage = CheckError.analyzeError(usedAlgorithm, page.getAnalysis(newContents, true));
          if ((errorPage != null) && (!errorPage.getErrorFound())) {
            checkWiki.markAsFixed(page, usedAlgorithm.getErrorNumberString());
            if (selectedAlgorithms.contains(usedAlgorithm)) {
              countMarked++;
            } else {
              countMarkedOther++;
            }
          }
        }
      } else if (analyzeNonFixed) {
        Controller.runFullAnalysis(page.getTitle(), null, getWikipedia());
      }
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

}
