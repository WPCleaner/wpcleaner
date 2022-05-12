/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;


/**
 * Tools for updating unknown parameter warnings.
 */
public class UpdateUnknownParameterWarningTools extends UpdateWarningTools {

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  public UpdateUnknownParameterWarningTools(
      EnumWikipedia wiki, BasicWorker worker,
      boolean createWarning, boolean automaticEdit) {
    this(wiki, worker, (worker != null) ? worker.getWindow() : null, createWarning, automaticEdit);
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   */
  public UpdateUnknownParameterWarningTools(EnumWikipedia wiki, BasicWindow window, boolean createWarning) {
    this(wiki, null, window, createWarning, false);
  }

  /**
   * @param wiki Wiki.
   * @param worker Worker.
   * @param window Window.
   * @param createWarning Create warning if necessary.
   * @param automaticEdit True if the edits are automatic.
   */
  private UpdateUnknownParameterWarningTools(
      EnumWikipedia wiki,
      BasicWorker worker, BasicWindow window,
      boolean createWarning, boolean automaticEdit) {
    super(wiki, worker, window, new UnknownParameterWarningProcessor(wiki), createWarning, automaticEdit);
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
}
