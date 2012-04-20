/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.worker;

import java.util.Collection;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.Contributions;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
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
  private final boolean updateWarning;
  private final boolean createWarning;
  private final Contributions contributions;
  private final Collection<CheckErrorAlgorithm> errorsFixed;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param page Page.
   * @param text Page contents.
   * @param comment Comment.
   * @param forceWatch Force watching the page.
   * @param updateWarning Update warning on talk page.
   * @param createWarning Create warning on talk page.
   * @param contributions Contributions.
   * @param errorsFixed Errors fixed by this update.
   */
  public SendWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      Page page, String text, String comment,
      boolean forceWatch,
      boolean updateWarning, boolean createWarning,
      Contributions contributions,
      Collection<CheckErrorAlgorithm> errorsFixed) {
    super(wikipedia, window);
    this.page = page;
    this.text = text;
    this.comment = comment;
    this.forceWatch = forceWatch;
    this.updateWarning = updateWarning;
    this.createWarning = createWarning;
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
    setText(GT._("Updating page contents"));
    int attemptNumber = 0;
    do {
      try {
        attemptNumber++;
        queryResult = api.updatePage(
            getWikipedia(), page, text,
            getWikipedia().createUpdatePageComment(comment, null),
            forceWatch);
      } catch (APIException e) {
        if ((e.getQueryResult() == EnumQueryResult.BAD_TOKEN) && (attemptNumber < 2)) {
          // Bad Token : Retrieve contents and try again
          setText(GT._(
              "Error {0} detected: Waiting and retrying",
              "'" + e.getErrorCode() + "'"));
          try {
            Page tmpPage = page.replicatePage();
            api.retrieveContents(getWikipedia(), tmpPage, false);
            page.setEditToken(tmpPage.getEditToken());
          } catch (APIException e2) {
            return e;
          }
        } else {
          return e;
        }
      }
    } while (queryResult == null);

    // Take contributions into account
    if ((contributions != null) &&
        (getWikipedia().getContributions() != null)) {
      getWikipedia().getContributions().increaseContributions(contributions);
    }

    // Updating disambiguation warning
    if (updateWarning) {
      try {
        UpdateDabWarningTools dabWarningTools = new UpdateDabWarningTools(
            getWikipedia(), this, createWarning);
        dabWarningTools.updateDabWarning(page, queryResult.getPageNewRevId(), text);
      } catch (APIException e) {
        return e;
      }
    }

    // Mark errors fixed
    if (errorsFixed != null) {
      for (CheckErrorAlgorithm error: errorsFixed) {
        if (error.getPriority() != CWConfigurationError.PRIORITY_BOT_ONLY) {
          OnePageWindow.markPageAsFixed(null, error.getErrorNumberString(), page);
        }
      }
    }

    return null;
  }
}