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
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Properties;

import org.apache.commons.httpclient.NameValuePair;
import org.wikipediacleaner.api.MediaWiki;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorComparator;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

/**
 * SwingWorker for reloading the page. 
 */
public class CheckWikiProjectWorker extends BasicWorker {

  private final ArrayList<CheckError> errors;
  private final boolean retrieveConfig;
  private final Properties checkWikiConfig;
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
   * @param properties Properties to complete.
   * @param onlyErrors If set, limit the errors retrieved to this error numbers.
   * @param exceptErrors If set, ignore this error numbers.
   * @param otherErrors Flag indicating if other errors should be retrieved (without list).
   * @param errorLimit Maximum number of pages that should be returned for each error.
   */
  public CheckWikiProjectWorker(
      EnumWikipedia wikipedia, BasicWindow window,
      ArrayList<CheckError> errors,
      Properties properties,
      ArrayList<Integer> onlyErrors,
      ArrayList<Integer> exceptErrors,
      boolean otherErrors,
      int errorLimit) {
    super(wikipedia, window);
    this.errors = errors;
    if (onlyErrors == null) {
      this.errors.clear();
    }
    this.retrieveConfig = ((onlyErrors == null) || (properties == null) || (otherErrors));
    this.checkWikiConfig = properties;
    this.onlyErrors = onlyErrors;
    this.exceptErrors = exceptErrors;
    this.otherErrors = otherErrors;
    this.errorLimit = errorLimit;
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
          int posEnd = line.indexOf(" END");
          while (posEnd == -1) {
            String nextLine = reader.readLine();
            if (nextLine != null) {
              line += "\n" + nextLine;
              posEnd = line.indexOf(" END");
            } else {
              posEnd = line.length();
            }
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

    // Retrieving Check Wiki configuration
    String code = getWikipedia().getCode().replace("-", "_");
    if (retrieveConfig) {
      try {
        setText(GT._("Retrieving Check Wiki configuration"));
        InputStream stream = APIFactory.getAPI().askToolServerGet(
            "~sk/checkwiki/" + code + "wiki/" + code + "wiki_translation.txt",
            true);
        if (stream != null) {
          BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
          while (getNextCheckWikiParameter(checkWikiConfig, reader)) {
            //
          }
          try {
            reader.close();
          } catch (IOException e) {
            // Nothing
          }
        }
        if (getWikipedia().getCheckWikiTraduction() != null) {
          MediaWiki mw = MediaWiki.getMediaWikiAccess(this);
          Page page = DataManager.getPage(
              getWikipedia(), getWikipedia().getCheckWikiTraduction(), null, null);
          mw.retrieveContents(getWikipedia(), page, true, false, false);
          BufferedReader reader = new BufferedReader(new StringReader(page.getContents()));
          while (getNextCheckWikiParameter(checkWikiConfig, reader)) {
            //
          }
          try {
            reader.close();
          } catch (IOException e) {
            // Nothing
          }
        }
      } catch (APIException e) {
        return e;
      } catch (UnsupportedEncodingException e) {
        return e;
      }
    }

    // Retrieving errors
    DecimalFormat errorNumberFormat = new DecimalFormat("000");
    for (int errorNumber = 1; errorNumber < 100; errorNumber++) {
      int errorPriority = CheckError.getErrorPriority(checkWikiConfig, getWikipedia(), errorNumber);
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
      if ((CheckError.isPriorityActive(errorPriority)) && (needError)) {
        String className = CheckErrorAlgorithm.class.getName() + errorNumberFormat.format(errorNumber);
        try {
  
          // Checking if the error number is known by WikiCleaner
          Class.forName(className);
  
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
          CheckError.addCheckError(checkWikiConfig, errors, getWikipedia(), errorNumber, stream);
        } catch (ClassNotFoundException e) {
          // Not found: error not yet available in WikiCleaner.
          if (needList) {
            System.out.println("Error " + errorNumber + " is not yet available in WikiCleaner.");
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