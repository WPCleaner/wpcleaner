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

import java.io.InputStream;
import java.util.Collections;
import java.util.List;

import org.apache.commons.httpclient.NameValuePair;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorComparator;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for reloading the page. 
 */
public class CheckWikiProjectWorker extends BasicWorker {

  private final List<CheckError> errors;
  private final List<CheckErrorAlgorithm> selectedAlgorithms;
  private final int errorLimit;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param errors Error list to complete.
   * @param selectedAlgorithms List of algorithms.
   * @param otherErrors Flag indicating if other errors should be retrieved (without list).
   * @param errorLimit Maximum number of pages that should be returned for each error.
   */
  public CheckWikiProjectWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      List<CheckError> errors,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      boolean otherErrors,
      int errorLimit) {
    super(wikipedia, window);
    this.errors = errors;
    if (!otherErrors) {
      this.errors.clear();
    }
    this.selectedAlgorithms = selectedAlgorithms;
    this.errorLimit = errorLimit;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {

    // Retrieving errors
    boolean errorLoaded = false;
    APIException exception = null;
    String code = getWikipedia().getCheckWikiCode().replace("-", "_");
    if (selectedAlgorithms != null) {
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        try {
          
          if ((algorithm != null) &&
              (algorithm.isAvailable()) &&
              (algorithm.getPriority() != CheckErrorAlgorithms.PRIORITY_BOT_ONLY)) {
            // Retrieving list of pages for the error number
            NameValuePair[] parameters = new NameValuePair[] {
                new NameValuePair("id", algorithm.getErrorNumberString()),
                new NameValuePair("limit", Integer.toString(errorLimit)),
                new NameValuePair("offset", Integer.toString(0)),
                new NameValuePair("project", code),
                new NameValuePair("view", "bots")
            };
            InputStream stream = null;
            setText(
                GT._("Checking for errors n°{0}", Integer.toString(algorithm.getErrorNumber())) +
                " - " + algorithm.getShortDescriptionReplaced());
            stream = APIFactory.getAPI().askToolServerPost(
                "~sk/cgi-bin/checkwiki/checkwiki.cgi", parameters, true);
            CheckError.addCheckError(
                errors, getWikipedia(),
                Integer.valueOf(algorithm.getErrorNumberString()), stream);
            errorLoaded = true;
          }
        } catch (APIException e) {
          exception = e;
        }
      }
    }
    if (!errorLoaded && (exception != null)) {
      return exception;
    }

    // Sorting errors by priority
    setText(GT._("Sorting errors by priority"));
    Collections.sort(errors, new CheckErrorComparator());

    return null;
  }
}