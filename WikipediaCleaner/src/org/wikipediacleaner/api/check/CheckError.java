/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.api.check;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * An abstract class for managing errors defind in the check wikipedia project.
 */
public class CheckError {

  /**
   * Analyze a page to find error types.
   * 
   * @param errors Possible error types.
   * @param page Page to be analyzed.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Error types found in the page.
   */
  public static ArrayList<CheckErrorAlgorithm> analyzeErrors(
      ArrayList<CheckError> errors, Page page, String contents) {
    ArrayList<CheckErrorAlgorithm> errorsFound = new ArrayList<CheckErrorAlgorithm>();
    if ((errors != null) && (page != null)) {
      if (contents == null) {
        contents = page.getContents();
      }
      for (CheckError error : errors) {
        if (error.algorithm != null) {
          if (error.algorithm.analyze(page, contents, null)) {
            errorsFound.add(error.algorithm);
          }
        }
      }
    }
    return errorsFound;
  }

  /**
   * Analyze a page to find errors of a given type.
   * 
   * @param algorithm Algorithm.
   * @param page Page to be analyzed.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Errors found in the page.
   */
  public static ArrayList<CheckErrorResult> analyzeError(
      CheckErrorAlgorithm algorithm, Page page, String contents) {
    ArrayList<CheckErrorResult> errorsFound = new ArrayList<CheckErrorResult>();
    if (algorithm != null) {
      algorithm.analyze(page, contents, errorsFound);
    }
    return errorsFound;
  }

  /**
   * @param errors Errors list.
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @param stream Stream containing list of pages for the error number.
   */
  public static void addCheckError(
      ArrayList<CheckError> errors,
      EnumWikipedia wikipedia, int errorNumber, InputStream stream) {
    CheckError error = new CheckError(wikipedia, errorNumber);
    boolean errorFound = false;
    try {
      BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
      String line = null;
      // TODO: Correctly parse HTML ?
      while (((line = reader.readLine()) != null) && !line.endsWith("<pre>")) {
        // Waiting for <pre>
      }
      while (((line = reader.readLine()) != null) && !line.startsWith("</pre>")) {
        // TODO: Use something like Apache Commons Lang StringEscapeUtils ?
        error.addPage(line);
        errorFound = true;
      }
    } catch (UnsupportedEncodingException e) {
      //
    } catch (IOException e) {
      //
    }
    if (errorFound) {
      errors.add(error);
    }
  }

  private final EnumWikipedia wikipedia;
  private final int errorNumber;
  private final CheckErrorAlgorithm algorithm;
  private final ArrayList<Page> errors;
  private boolean fullErrorsInitialized;
  private final ArrayList<Page> fullErrors;

  /**
   * Constructor
   * 
   * @param errorNumber Error number as defined in the check wikipedia project.
   */
  private CheckError(EnumWikipedia wikipedia, int errorNumber) {
    this.wikipedia = wikipedia;
    String className = CheckErrorAlgorithm.class.getName() + Integer.toString(errorNumber);
    CheckErrorAlgorithm tmpAlgorithm = null;
    try {
      Class algorithmClass = Class.forName(className);
      tmpAlgorithm = (CheckErrorAlgorithm) algorithmClass.newInstance();
    } catch (ClassNotFoundException e) {
      // Not found: error not yet available in WikiCleaner.
    } catch (InstantiationException e) {
      System.err.println("InstantiationException for " + className);
    } catch (IllegalAccessException e) {
      System.err.println("IllegalAccessException for " + className);
    } catch (ClassCastException e) {
      System.err.println(
          "Class " + className +
          " doesn't implement " + CheckErrorAlgorithm.class.getName());
    }
    this.algorithm = tmpAlgorithm;
    this.errorNumber = errorNumber;
    this.errors = new ArrayList<Page>();
    this.fullErrors = new ArrayList<Page>();
    this.fullErrorsInitialized = false;
  }
  
  /**
   * @return Error number as defined in the check wikipedia project.
   */
  public int getErrorNumber() {
    return errorNumber;
  }

  /**
   * @return Flag indicating if the full list is available.
   */
  public boolean isFullListInitialized() {
    return fullErrorsInitialized;
  }

  /**
   * @return Number of error pages.
   */
  public int getPageCount(boolean full) {
    if (full && fullErrorsInitialized) {
      return fullErrors.size();
    }
    return errors.size();
  }

  /**
   * @param index Page index.
   * @return Error page.
   */
  public Page getPage(int index, boolean full) {
    if (full && fullErrorsInitialized) {
      if ((index < 0) || (index >= fullErrors.size())) {
        return null;
      }
      return fullErrors.get(index);
    }
    if ((index < 0) || (index >= errors.size())) {
      return null;
    }
    return errors.get(index);
  }

  /**
   * Add a page to the list of errors.
   * 
   * @param page Page.
   */
  private void addPage(String page) {
    Page tmpPage = DataManager.getPage(wikipedia, page, null, null);
    if (!errors.contains(tmpPage)) {
      errors.add(tmpPage);
    }
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    String count;
    if (fullErrorsInitialized) {
      count = Integer.toString(errors.size()) + "/" + Integer.toString(fullErrors.size());
    } else {
      count = Integer.toString(errors.size());
    }
    return GT._("Error n°{0} ({1}) - {2}", new Object[] {
        Integer.valueOf(errorNumber),
        count,
        (algorithm != null) ?
            algorithm.getErrorDescription() :
            GT._("Error unkown from WikiCleaner") });
  }
}
