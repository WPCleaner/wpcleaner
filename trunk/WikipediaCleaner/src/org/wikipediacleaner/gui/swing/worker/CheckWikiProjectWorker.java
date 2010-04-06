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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Properties;

import org.apache.commons.httpclient.NameValuePair;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.CheckErrorComparator;
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
  private final boolean retrieveConfig;
  private final Properties checkWikiConfig;
  private final Integer onlyError;

  /**
   * Constructor.
   * 
   * @param wikipedia Wikipedia.
   * @param window Window.
   * @param errors Error list to complete.
   * @param properties Properties to complete.
   * @param onlyError If set, limit the errors retrieved to this error number.
   */
  public CheckWikiProjectWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      ArrayList<CheckError> errors,
      Properties properties,
      Integer onlyError) {
    super(wikipedia, window);
    this.errors = errors;
    if (onlyError == null) {
      this.errors.clear();
    }
    this.retrieveConfig = true;
    this.checkWikiConfig = properties;
    this.onlyError = onlyError;
  }

  /**
   * Extract next parameter from Check Wiki configuration.
   *
   * @param properties Properties to store the next parameter.
   * @param reader Reader for the configuration.
   * @return Next parameter found.
   * @throw APIException.
   */
  private boolean getNextCheckWikiParameter(
      Properties properties, BufferedReader reader) throws APIException {
    String line;
    try {
      while ((line = reader.readLine()) != null) {
        int posEqual = line.indexOf('=');
        if (posEqual > 0) {
          String name = line.substring(0, posEqual);
          line = line.substring(posEqual + 1);
          int posEnd;
          while ((posEnd = line.indexOf(" END")) == -1) {
            line += "\n" + reader.readLine();
          }
          line = line.substring(0, posEnd);
          properties.setProperty(name.trim(), line);
          return true;
        }
      }
    } catch (IOException e) {
      throw new APIException("Error reading Check Wiki configuration: " + e.getMessage());
    }
    return false;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    Configuration config = Configuration.getConfiguration();

    // Retrieving Check Wiki configuration
    String code = getWikipedia().getCode();
    if (retrieveConfig) {
      try {
        setText(GT._("Retrieving Check Wiki configuration"));
        InputStream stream = APIFactory.getAPI().askToolServerGet(
            "~sk/checkwiki/" + code + "wiki/" + code + "wiki_translation.txt",
            true);
        BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
        while (getNextCheckWikiParameter(checkWikiConfig, reader)) {
          //
        }
      } catch (APIException e) {
        return e;
      } catch (UnsupportedEncodingException e) {
        return e;
      }
    }

    // Retrieving errors
    for (int errorNumber = 1; errorNumber < 100; errorNumber++) {
      int errorPriority = CheckError.getErrorPriority(checkWikiConfig, getWikipedia(), errorNumber);
      if ((CheckError.isPriorityActive(errorPriority)) &&
          ((onlyError == null) || (onlyError.intValue() == errorNumber))) {
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
              new NameValuePair("project", code + "wiki"),
              new NameValuePair("view", "bots")
          };
          InputStream stream = APIFactory.getAPI().askToolServerPost(
              "~sk/cgi-bin/checkwiki/checkwiki.cgi", parameters, true);
          CheckError.addCheckError(checkWikiConfig, errors, getWikipedia(), errorNumber, stream);
        } catch (ClassNotFoundException e) {
          // Not found: error not yet available in WikiCleaner.
        } catch (APIException e) {
          return e;
        }
      }
    }

    // Sorting errors by priority
    Collections.sort(errors, new CheckErrorComparator());
    return null;
  }
}