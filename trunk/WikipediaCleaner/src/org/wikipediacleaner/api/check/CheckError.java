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
   * Create new instances of CheckError after parsing the current status of the check wikipedia project.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents of the check wikipedia project.
   * @return Array of CheckError.
   */
  public static ArrayList<CheckError> initCheckErrors(EnumWikipedia wikipedia, String contents) {
    if (contents == null) {
      return null;
    }
    ArrayList<CheckError> result = new ArrayList<CheckError>();
    int beginIndex = 0;
    final String beginError = "<!-- error number ";
    while (beginIndex < contents.length()) {
      beginIndex = contents.indexOf(beginError, beginIndex);
      if (beginIndex < 0) {
        beginIndex = contents.length();
      } else {
        beginIndex += beginError.length();
        int endIndex = beginIndex;
        while (Character.isDigit(contents.charAt(endIndex))) {
          endIndex++;
        }
        if (endIndex > beginIndex) {
          CheckError error = new CheckError(wikipedia, Integer.parseInt(contents.substring(beginIndex, endIndex)));
          int nextChapter = contents.indexOf("\n=", endIndex);
          int nextError = contents.indexOf(beginError, endIndex);
          endIndex = Math.min(
              (nextChapter < 0) ? contents.length() : nextChapter,
              (nextError < 0) ? contents.length() : nextError);
          while (beginIndex < endIndex) {
            beginIndex = contents.indexOf("\n", beginIndex);
            if ((beginIndex < 0) || (beginIndex >= endIndex)) {
              beginIndex = endIndex;
            } else {
              beginIndex++;
              if (contents.startsWith("| [[:", beginIndex)) {
                beginIndex += 5;
                error.addPage(contents.substring(beginIndex, contents.indexOf("]]", beginIndex)));
              }
            }
          }
          result.add(error);
        }
      }
    }
    return result;
  }

  /**
   * Analyze a page to find errors.
   * 
   * @param errors Possible errors.
   * @param page Page to be analyzed.
   * @return Errors found in the page.
   */
  public static ArrayList<CheckErrorAlgorithm> analyzeErrors(
      ArrayList<CheckError> errors, Page page) {
    ArrayList<CheckErrorAlgorithm> errorsFound = new ArrayList<CheckErrorAlgorithm>();
    if ((errors != null) && (page != null)) {
      for (CheckError error : errors) {
        if (error.algorithm != null) {
          if (error.algorithm.analyze(page)) {
            errorsFound.add(error.algorithm);
          }
        }
      }
    }
    return errorsFound;
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
    Page tmpPage = DataManager.getPage(wikipedia, page, null);
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
