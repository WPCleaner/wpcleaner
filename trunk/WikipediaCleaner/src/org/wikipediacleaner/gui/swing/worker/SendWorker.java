/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collection;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.Contributions;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.gui.swing.OnePageWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for sending the new page content. 
 */
public class SendWorker extends BasicWorker {

  private final Page page;
  private final String text;
  private final String comment;
  private final boolean forceWatch;
  private final boolean updateDabWarning;
  private final boolean createDabWarning;
  private final boolean updateISBNWarning;
  private final boolean createISBNWarning;
  private final boolean updateDuplicateArgsWarning;
  private final boolean createDuplicateArgsWarning;
  private final Contributions contributions;
  private final Collection<CheckErrorAlgorithm> errorsFixed;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   * @param text Page contents.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @param updateDabWarning Update disambiguation warning on talk page.
   * @param createDabWarning Create disambiguation warning on talk page.
   * @param updateISBNWarning Update ISBN warning on talk page.
   * @param createISBNWarning Create ISBN warning on talk page.
   * @param updateDuplicateArgsWarning Update duplicate arguments warning on talk page.
   * @param createDuplicateArgsWarning Create duplicate arguments warning on talk page.
   * @param contributions Contributions.
   * @param errorsFixed Errors fixed by this update.
   */
  public SendWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      Page page, String text, String comment,
      boolean forceWatch,
      boolean updateDabWarning, boolean createDabWarning,
      boolean updateISBNWarning, boolean createISBNWarning,
      boolean updateDuplicateArgsWarning, boolean createDuplicateArgsWarning,
      Contributions contributions,
      Collection<CheckErrorAlgorithm> errorsFixed) {
    super(wikipedia, window);
    this.page = page;
    this.text = text;
    this.comment = comment;
    this.forceWatch = forceWatch;
    this.updateDabWarning = updateDabWarning;
    this.createDabWarning = createDabWarning;
    this.updateISBNWarning = updateISBNWarning;
    this.createISBNWarning = createISBNWarning;
    this.updateDuplicateArgsWarning = updateDuplicateArgsWarning;
    this.createDuplicateArgsWarning = createDuplicateArgsWarning;
    this.contributions = contributions;
    this.errorsFixed = errorsFixed;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    setText(GT._("Retrieving MediaWiki API"));
    API api = APIFactory.getAPI();

    // Updating page contents
    QueryResult queryResult = null;
    try {
      setText(GT._("Updating page contents"));
      queryResult = api.updatePage(
          getWikipedia(), page, text,
          getWikipedia().createUpdatePageComment(comment, null, false),
          forceWatch);
    } catch (APIException e) {
      return e;
    }

    // Take contributions into account
    if ((contributions != null) &&
        (getWikipedia().getContributions() != null)) {
      getWikipedia().getContributions().increaseContributions(contributions);
    }

    // Updating disambiguation warning
    if (updateDabWarning) {
      try {
        UpdateDabWarningTools dabWarningTools = new UpdateDabWarningTools(
            getWikipedia(), this, createDabWarning, false);
        PageAnalysis pageAnalysis = page.getAnalysis(text, true);
        dabWarningTools.updateWarning(
            pageAnalysis, queryResult.getPageNewRevId(),
            null, null, null, null, null);
      } catch (APIException e) {
        return e;
      }
    }

    // Updating ISBN warning
    if (updateISBNWarning) {
      try {
        UpdateISBNWarningTools isbnWarningTools = new UpdateISBNWarningTools(
            getWikipedia(), this, createISBNWarning, false);
        PageAnalysis pageAnalysis = page.getAnalysis(text, true);
        isbnWarningTools.updateWarning(
            pageAnalysis, queryResult.getPageNewRevId(),
            null, null, null, null, null);
      } catch (APIException e) {
        return e;
      }
    }

    // Updating duplicate arguments warning
    if (updateDuplicateArgsWarning) {
      try {
        UpdateDuplicateArgsWarningTools duplicateArgsWarningTools = new UpdateDuplicateArgsWarningTools(
            getWikipedia(), this, createDuplicateArgsWarning, false);
        PageAnalysis pageAnalysis = page.getAnalysis(text, true);
        duplicateArgsWarningTools.updateWarning(
            pageAnalysis, queryResult.getPageNewRevId(),
            null, null, null, null, null);
      } catch (APIException e) {
        return e;
      }
    }

    // Mark errors fixed
    if (errorsFixed != null) {
      for (CheckErrorAlgorithm error: errorsFixed) {
        if (error.getPriority() != CWConfigurationError.PRIORITY_BOT_ONLY) {
          OnePageWindow.markPageAsFixed(error.getErrorNumberString(), page);
        }
      }
    }

    return null;
  }
}