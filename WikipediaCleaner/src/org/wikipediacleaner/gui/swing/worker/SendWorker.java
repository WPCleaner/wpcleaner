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

  /**
   * Utility class for storing SendWorker parameters.
   */
  private static class Params {
    protected boolean updateDabWarning;
    protected boolean createDabWarning;
    protected boolean updateISBNWarning;
    protected boolean createISBNWarning;
    protected boolean updateISSNWarning;
    protected boolean createISSNWarning;
    protected boolean updateDuplicateArgsWarning;
    protected boolean createDuplicateArgsWarning;

    protected Params() {
      //
    }
  }

  /**
   * Utility class for building a SendWorker.
   */
  public static class Builder {
    private Params params;

    /**
     * Prepare for the creation of a SendWorker.
     */
    public Builder() {
      params = new Params();
    }

    /**
     * @param update Allow/Deny update of disambiguation warning.
     * @param create Allow/Deny creation of disambiguation warning.
     * @return Builder itself.
     */
    public Builder allowDabWarning(boolean update, boolean create) {
      params.updateDabWarning = update;
      params.createDabWarning = create;
      return this;
    }

    /**
     * @param update Allow/Deny update of ISBN warning.
     * @param create Allow/Deny creation of ISBN warning.
     * @return Builder itself.
     */
    public Builder allowISBNWarning(boolean update, boolean create) {
      params.updateISBNWarning = update;
      params.createISBNWarning = create;
      return this;
    }

    /**
     * @param update Allow/Deny update of ISSN warning.
     * @param create Allow/Deny creation of ISSN warning.
     * @return Builder itself.
     */
    public Builder allowISSNWarning(boolean update, boolean create) {
      params.updateISSNWarning = update;
      params.createISSNWarning = create;
      return this;
    }

    /**
     * @param update Allow/Deny update of duplicate arguments warning.
     * @param create Allow/Deny creation of duplicate arguments warning.
     * @return Builder itself.
     */
    public Builder allowDuplicateArgsWarning(boolean update, boolean create) {
      params.updateDuplicateArgsWarning = update;
      params.createDuplicateArgsWarning = create;
      return this;
    }

    /**
     * @param wiki Wiki.
     * @param window Window.
     * @param page Page.
     * @param text Page contents.
     * @param comment Comment.
     * @param forceWatch Force watching the page.
     * @param params Parameters.
     * @param contributions Contributions.
     * @param errorsFixed Errors fixed by this update.
     */
    public SendWorker createWorker(
        EnumWikipedia wiki, BasicWindow window,
        Page page, String text, String comment,
        boolean forceWatch,
        Contributions contributions,
        Collection<CheckErrorAlgorithm> errorsFixed) {
      return new SendWorker(wiki, window, page, text, comment, forceWatch, params, contributions, errorsFixed);
    }
  }

  private final Page page;
  private final String text;
  private final String comment;
  private final boolean forceWatch;
  private final Params params;
  private final Contributions contributions;
  private final Collection<CheckErrorAlgorithm> errorsFixed;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param page Page.
   * @param text Page contents.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @param params Parameters.
   * @param contributions Contributions.
   * @param errorsFixed Errors fixed by this update.
   */
  SendWorker(
      EnumWikipedia wiki, BasicWindow window,
      Page page, String text, String comment,
      boolean forceWatch,
      Params params,
      Contributions contributions,
      Collection<CheckErrorAlgorithm> errorsFixed) {
    super(wiki, window);
    this.page = page;
    this.text = text;
    this.comment = comment;
    this.forceWatch = forceWatch;
    this.params = params;
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
          comment,
          false, forceWatch);
    } catch (APIException e) {
      return e;
    }

    // Take contributions into account
    if ((contributions != null) &&
        (getWikipedia().getContributions() != null)) {
      getWikipedia().getContributions().increaseContributions(contributions);
    }

    // Updating disambiguation warning
    if (params.updateDabWarning) {
      try {
        UpdateDabWarningTools dabWarningTools = new UpdateDabWarningTools(
            getWikipedia(), this, params.createDabWarning, false);
        PageAnalysis pageAnalysis = page.getAnalysis(text, true);
        dabWarningTools.updateWarning(
            pageAnalysis, queryResult.getPageNewRevId(),
            null, null, null, null, null);
      } catch (APIException e) {
        return e;
      }
    }

    // Updating ISBN warning
    if (params.updateISBNWarning) {
      try {
        UpdateISBNWarningTools isbnWarningTools = new UpdateISBNWarningTools(
            getWikipedia(), this, params.createISBNWarning, false);
        PageAnalysis pageAnalysis = page.getAnalysis(text, true);
        isbnWarningTools.updateWarning(
            pageAnalysis, queryResult.getPageNewRevId(),
            null, null, null, null, null);
      } catch (APIException e) {
        return e;
      }
    }

    // Updating ISSN warning
    if (params.updateISSNWarning) {
      try {
        UpdateISSNWarningTools issnWarningTools = new UpdateISSNWarningTools(
            getWikipedia(), this, params.createISSNWarning, false);
        PageAnalysis pageAnalysis = page.getAnalysis(text, true);
        issnWarningTools.updateWarning(
            pageAnalysis, queryResult.getPageNewRevId(),
            null, null, null, null, null);
      } catch (APIException e) {
        return e;
      }
    }

    // Updating duplicate arguments warning
    if (params.updateDuplicateArgsWarning) {
      try {
        UpdateDuplicateArgsWarningTools duplicateArgsWarningTools = new UpdateDuplicateArgsWarningTools(
            getWikipedia(), this, params.createDuplicateArgsWarning, false);
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