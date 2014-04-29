/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISBN;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationBoolean;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating ISBN warnings.
 */
public class UpdateISBNWarningTools extends UpdateWarningTools {

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateISBNWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateISBNWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateISBNWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, createWarning, automaticEdit);
  }

  /**
   * Retrieve information in the pages to construct the warning.
   * 
   * @param pages List of pages.
   * @throws APIException
   */
  @Override
  protected void retrievePageInformation(
      List<Page> pages) throws APIException {

    // Retrieving page contents
    if (!getContentsAvailable()) {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);
      mw.retrieveContents(wiki, pages, true, false, false, true);
    }
  }

  /**
   * Extract information about ISBN with errors.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of ISBN errors).
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return List of ISBN errors.
   */
  @Override
  protected Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }

    // Prepare list of algorithms
    List<CheckErrorAlgorithm> algorithms = new ArrayList<CheckErrorAlgorithm>();
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 70)); // Incorrect length
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 71)); // Incorrect X
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 72)); // Incorrect ISBN-10
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 73)); // Incorrect ISBN-13

    // Retrieve list of errors
    List<CheckErrorResult> errorResults = new ArrayList<CheckErrorResult>();
    for (CheckErrorAlgorithm algorithm : algorithms) {
      int errorNumber = algorithm.getErrorNumber();
      if (CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber)) {
        CheckErrorPage errorPage = CheckError.analyzeError(algorithm, analysis);
        List<CheckErrorResult> results = errorPage.getResults();
        if (results != null) {
          errorResults.addAll(results);
        }
      }
    }
    Collections.sort(errorResults);

    // Compute list of elements for the warning
    List<String> elements = new ArrayList<String>();
    int pos = 0;
    while (pos < errorResults.size()) {
      CheckErrorResult errorResult = errorResults.get(pos);
      int beginIndex = errorResult.getStartPosition();
      int endIndex = errorResult.getEndPosition();
      int next = pos + 1;
      while ((next < errorResults.size()) &&
             (beginIndex == errorResults.get(next).getStartPosition()) &&
             (endIndex == errorResults.get(next).getEndPosition())) {
        next++;
      }
      String error = analysis.getContents().substring(beginIndex, endIndex);
      error = error.replaceAll("\\=", "&#x3D;"); // Replace "=" by its HTML value
      error = error.replaceAll("\\n", "\u21b5"); // Replacer \n by a visual character
      elements.add(error);
      memorizeError(error, analysis.getPage().getTitle());
      StringBuilder comment = new StringBuilder();
      while (pos < next) {
        errorResult = errorResults.get(pos);
        CheckErrorAlgorithm algorithm = errorResult.getAlgorithm();
        PageElementISBN isbn = analysis.isInISBN(beginIndex);
        if ((algorithm != null) &&
            (algorithm instanceof CheckErrorAlgorithmISBN) &&
            (isbn != null)) {
          CheckErrorAlgorithmISBN isbnAlgo = (CheckErrorAlgorithmISBN) algorithm;
          String reason = isbnAlgo.getReason(isbn);
          if ((reason != null) && (reason.length() > 0)) {
            if (comment.length() > 0) {
              comment.append(" - ");
            }
            comment.append(reason);
          }
        }
        pos++;
      }
      elements.add(comment.toString());
    }
    return elements;
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  @Override
  protected WPCConfigurationString getWarningTemplate() {
    return WPCConfigurationString.ISBN_WARNING_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the warning template comment.
   */
  @Override
  protected WPCConfigurationString getWarningTemplateComment() {
    return WPCConfigurationString.ISBN_WARNING_TEMPLATE_COMMENT;
  }

  /**
   * @return True if section 0 of the talk page should be used.
   */
  @Override
  protected boolean useSection0() {
    return configuration.getBoolean(WPCConfigurationBoolean.ISBN_WARNING_SECTION_0);
  }

  /**
   * @return Comment when warning is removed.
   */
  @Override
  protected String getWarningCommentDone() {
    return configuration.getISBNWarningCommentDone();
  }

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  @Override
  protected String getWarningComment(Collection<String> elements) {
    Collection<String> isbns = new ArrayList<String>();
    int i = 0;
    for (String element : elements) {
      if (i % 2 == 0) {
        isbns.add(element);
      }
      i++;
    }
    return configuration.getISBNWarningComment(isbns);
  }

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  @Override
  protected String getMessageRemoveWarning(String title) {
    return GT._("Removing ISBN warning - {0}", title);
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._("Updating ISBN warning - {0}", title);
  }
}
