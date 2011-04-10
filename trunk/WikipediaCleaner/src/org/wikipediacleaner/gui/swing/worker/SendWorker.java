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
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.QueryResult;
import org.wikipediacleaner.gui.swing.PageWindow;
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
   * @param errorsFixed Errors fixed by this update.
   */
  public SendWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      Page page, String text, String comment,
      boolean forceWatch,
      boolean updateWarning, boolean createWarning,
      Collection<CheckErrorAlgorithm> errorsFixed) {
    super(wikipedia, window);
    this.page = page;
    this.text = text;
    this.comment = comment;
    this.forceWatch = forceWatch;
    this.updateWarning = updateWarning;
    this.createWarning = createWarning;
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
          getWikipedia().createUpdatePageComment(comment, null),
          forceWatch);
    } catch (APIException e) {
      switch (e.getQueryResult()) {
      case BAD_TOKEN:
        // Bad Token : Retrieve contents and try again
        try {
          setText(GT._("Error 'badtoken' detected: Retrying"));
          api.retrieveContents(getWikipedia(), page, false);
          queryResult = api.updatePage(
              getWikipedia(), page, text,
              getWikipedia().createUpdatePageComment(comment, null),
              forceWatch);
        } catch (APIException e2) {
          return e2;
        }
        break;

      case READ_ONLY:
        // Read Only : Wait a few seconds before retrying
        try {
          setText(GT._("Error 'readonly' detected: Waiting and retrying"));
          try {
            Thread.sleep(10000);
          } catch (InterruptedException e1) {
            // Nothing to do 
          }
          queryResult = api.updatePage(
              getWikipedia(), page, text,
              getWikipedia().createUpdatePageComment(comment, null),
              forceWatch);
        } catch (APIException e2) {
          return e2;
        }
        break;

      default:
        return e;
      }
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
        PageWindow.markPageAsFixed(null, error.getErrorNumberString(), page);
      }
    }

    return null;
  }
}