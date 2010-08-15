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
import java.util.ArrayList;
import java.util.Collections;

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

  private final ArrayList<CheckError> errors;
  private final ArrayList<Integer> onlyErrors;
  private final ArrayList<Integer> exceptErrors;
  private final boolean otherErrors;
  private final int errorLimit;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param errors Error list to complete.
   * @param onlyErrors If set, limit the errors retrieved to this error numbers.
   * @param exceptErrors If set, ignore this error numbers.
   * @param otherErrors Flag indicating if other errors should be retrieved (without list).
   * @param errorLimit Maximum number of pages that should be returned for each error.
   */
  public CheckWikiProjectWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      ArrayList<CheckError> errors,
      ArrayList<Integer> onlyErrors,
      ArrayList<Integer> exceptErrors,
      boolean otherErrors,
      int errorLimit) {
    super(wikipedia, window);
    this.errors = errors;
    if (onlyErrors == null) {
      this.errors.clear();
    }
    this.onlyErrors = onlyErrors;
    this.exceptErrors = exceptErrors;
    this.otherErrors = otherErrors;
    this.errorLimit = errorLimit;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {

    // Retrieving errors
    String code = getWikipedia().getCode().replace("-", "_");
    for (int errorNumber = 1; errorNumber < 100; errorNumber++) {
      int errorPriority = CheckErrorAlgorithms.getPriority(getWikipedia(), errorNumber);
      boolean needError = false;
      boolean needList = false;
      if ((exceptErrors != null) && (exceptErrors.contains(Integer.valueOf(errorNumber)))) {
        // Nothing to do 
      } else if ((onlyErrors == null) || (onlyErrors.contains(Integer.valueOf(errorNumber)))) {
        needError = true;
        needList = true;
      } else if (otherErrors) {
        needError = true;
      }
      if ((CheckErrorAlgorithms.isPriorityActive(errorPriority)) && (needError)) {
        try {
  
          // Checking if the error number is known by WikiCleaner
          CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(getWikipedia(), errorNumber);

          if ((algorithm != null) && (algorithm.isAvailable())) {
            // Retrieving list of pages for the error number
            NameValuePair[] parameters = new NameValuePair[] {
                new NameValuePair("id", Integer.toString(errorNumber)),
                new NameValuePair("limit", Integer.toString(errorLimit)),
                new NameValuePair("offset", Integer.toString(0)),
                new NameValuePair("project", code + "wiki"),
                new NameValuePair("view", "bots")
            };
            InputStream stream = null;
            if (needList) {
              setText(GT._("Checking for errors n°{0}", Integer.toString(errorNumber)));
              stream = APIFactory.getAPI().askToolServerPost(
                  "~sk/cgi-bin/checkwiki/checkwiki.cgi", parameters, true);
            }
            CheckError.addCheckError(errors, getWikipedia(), errorNumber, stream);
          }
        } catch (APIException e) {
          return e;
        }
      }
    }

    // Sorting errors by priority
    setText(GT._("Sorting errors by priority"));
    Collections.sort(errors, new CheckErrorComparator());

    return null;
  }
}