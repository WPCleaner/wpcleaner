/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Tools for updating ISSN warnings.
 */
public class UpdateISSNWarningTools extends UpdateWarningTools {

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateISSNWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateISSNWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateISSNWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, new ISSNWarningProcessor(wiki), createWarning, automaticEdit);
  }

  /**
   * Retrieve information in the pages to construct the warning.
   * 
   * @param pages List of pages.
   * @return True if information was retrieved.
   * @throws APIException Exception thrown by the API.
   */
  @Override
  protected boolean retrievePageInformation(
      List<Page> pages) throws APIException {

    // Retrieving page contents
    if (!getContentsAvailable()) {
      MediaWiki mw = MediaWiki.getMediaWikiAccess(worker);
      mw.retrieveContents(wiki, pages, true, false, false, true);
    }

    return true;
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  @Override
  protected WPCConfigurationString getWarningTemplate() {
    return WPCConfigurationString.ISSN_WARNING_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the warning template comment.
   */
  @Override
  protected WPCConfigurationString getWarningTemplateComment() {
    return WPCConfigurationString.ISSN_WARNING_TEMPLATE_COMMENT;
  }

  /**
   * @return True if section 0 of the talk page should be used.
   */
  @Override
  protected boolean useSection0() {
    return configuration.getBoolean(WPCConfigurationBoolean.ISSN_WARNING_SECTION_0);
  }

  /**
   * @return Comment when warning is removed.
   */
  @Override
  protected String getWarningCommentDone() {
    return configuration.getISSNWarningCommentDone();
  }

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  @Override
  protected String getWarningComment(Collection<String> elements) {
    Collection<String> issns = new ArrayList<>();
    int i = 0;
    for (String element : elements) {
      if (i % 2 == 0) {
        issns.add(element);
      }
      i++;
    }
    return configuration.getISSNWarningComment(issns);
  }

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  @Override
  protected String getMessageRemoveWarning(String title) {
    return GT._T("Removing {1} warning - {0}", new Object[] { title, "ISSN" });
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._T("Updating {1} warning - {0}", new Object[] { title, "ISSN" });
  }
}
