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

import org.apache.commons.httpclient.NameValuePair;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;

/**
 * SwingWorker for reloading the page. 
 */
public class CheckWikiProjectWorker extends BasicWorker {

  private final ArrayList<CheckError> errors;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param errors Error list to complete.
   */
  public CheckWikiProjectWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      ArrayList<CheckError> errors) {
    super(wikipedia, window);
    this.errors = errors;
    this.errors.clear();
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    Configuration config = Configuration.getConfiguration();
    for (int errorNumber = 1; errorNumber < 100; errorNumber++) {
      String className = CheckErrorAlgorithm.class.getName() + Integer.toString(errorNumber);
      try {
        setText(GT._("Checking for errors n°{0}", Integer.toString(errorNumber)));

        // Checking if the error number is known by WikiCleaner
        Class.forName(className);

        // Retrieving list of pages for the error number
        NameValuePair[] parameters = new NameValuePair[] {
            new NameValuePair("id", Integer.toString(errorNumber)),
            new NameValuePair("limit", Integer.toString(config.getInt(
                                Configuration.INTEGER_CHECK_NB_ERRORS,
                                Configuration.DEFAULT_CHECK_NB_ERRORS))),
            new NameValuePair("offset", Integer.toString(0)),
            new NameValuePair("project", getWikipedia().getCode() + "wiki"),
            new NameValuePair("view", "bots")
        };
        InputStream stream = APIFactory.getAPI().askCheckWiki(parameters, true);
        CheckError.addCheckError(errors, getWikipedia(), errorNumber, stream);
      } catch (ClassNotFoundException e) {
        // Not found: error not yet available in WikiCleaner.
      } catch (APIException e) {
        return e;
      }
    }
    return null;
  }
}