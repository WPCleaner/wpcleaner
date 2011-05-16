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
import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.httpclient.NameValuePair;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.i18n.GT;


/**
 * An abstract class for managing errors defind in the check wikipedia project.
 */
public class CheckError {

  private static boolean traceTime = false;

  /**
   * Analyze a page to find error types.
   * 
   * @param algorithms Possible algorithms.
   * @param pageAnalysis Page analysis.
   * @return Errors found in the page.
   */
  public static List<CheckErrorPage> analyzeErrors(
      Collection<CheckErrorAlgorithm> algorithms, PageAnalysis pageAnalysis) {
    long beginAll = traceTime ? System.currentTimeMillis() : 0;
    List<CheckErrorPage> errorsFound = new ArrayList<CheckErrorPage>();
    if ((algorithms != null) && (pageAnalysis != null)) {
      for (CheckErrorAlgorithm algorithm : algorithms) {
        if ((algorithm != null) &&
            (algorithm.isAvailable()) &&
            (CheckErrorAlgorithms.isPriorityActive(algorithm.getPriority()))) {
          long beginOne = traceTime ? System.currentTimeMillis() : 0;
          List<CheckErrorResult> results = new ArrayList<CheckErrorResult>();
          if (algorithm.analyze(pageAnalysis, results)) {
            CheckErrorPage errorPage = new CheckErrorPage(pageAnalysis.getPage(), algorithm);
            errorPage.setResults(true, results);
            errorsFound.add(errorPage);
          }
          if (traceTime) {
            long endOne = System.currentTimeMillis();
            log("Error n°" + algorithm.getErrorNumber() + ": " + (endOne - beginOne));
          }
        }
      }
    }
    if (traceTime) {
      long endAll = System.currentTimeMillis();
      log("All: " + (endAll - beginAll));
    }
    return errorsFound;
  }

  /**
   * Analyze a page to find errors of a given type.
   * 
   * @param algorithm Algorithm.
   * @param pageAnalysis Page analysis.
   * @return Error page.
   */
  public static CheckErrorPage analyzeError(
      CheckErrorAlgorithm algorithm, PageAnalysis pageAnalysis) {
    if ((algorithm == null) || (pageAnalysis == null)) {
      return null;
    }
    long beginOne = (traceTime) ? System.currentTimeMillis() : 0;
    CheckErrorPage errorPage = new CheckErrorPage(pageAnalysis.getPage(), algorithm);
    boolean errorFound = false;
    List<CheckErrorResult> errorsFound = new ArrayList<CheckErrorResult>();
    errorFound = algorithm.analyze(pageAnalysis, errorsFound);
    errorPage.setResults(errorFound, errorsFound);
    if (traceTime) {
      long endOne = System.currentTimeMillis();
      log("Error n°" + algorithm.getErrorNumber() + ": " + (endOne - beginOne));
    }
    return errorPage;
  }

  /**
   * @param errors Errors list.
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @param stream Stream containing list of pages for the error number.
   */
  public static void addCheckError(
      List<CheckError> errors,
      EnumWikipedia wikipedia, int errorNumber, InputStream stream) {

    // Analyze properties to find infos about error number
    if (!CheckErrorAlgorithms.isAlgorithmActive(wikipedia, errorNumber)) {
      return;
    }

    // Create error
    CheckError error = new CheckError(wikipedia, errorNumber);
    if (stream != null) {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
        String line = null;
        // TODO: Correctly parse HTML ?
        while (((line = reader.readLine()) != null) && !line.endsWith("<pre>")) {
          // Waiting for <pre>
        }
        while (((line = reader.readLine()) != null) && !line.startsWith("</pre>")) {
          // TODO: Use something like Apache Commons Lang StringEscapeUtils ?
          line = line.replaceAll(Pattern.quote("&#039;"), "'");
          line = line.replaceAll(Pattern.quote("&quot;"), "\"");
          line = line.replaceAll(Pattern.quote("&amp;"), "&");
          error.addPage(line);
        }
      } catch (UnsupportedEncodingException e) {
        //
      } catch (IOException e) {
        //
      }
    }

    // Add / Replace error
    for (int i = errors.size(); i > 0; i--) {
      if (errors.get(i - 1).getErrorNumber() == errorNumber) {
        errors.remove(i - 1);
      }
    }
    errors.add(error);
  }

  /**
   * Log.
   * 
   * @param text String to log.
   */
  private static void log(String text) {
    System.out.println(text);
  }

  private final EnumWikipedia wikipedia;
  private final int errorNumber;
  private final CheckErrorAlgorithm algorithm;
  private final List<Page> errors;

  /**
   * Constructor
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number as defined in the check wikipedia project.
   */
  private CheckError(
      EnumWikipedia wikipedia, int errorNumber) {
    this.wikipedia = wikipedia;
    this.algorithm = CheckErrorAlgorithms.getAlgorithm(wikipedia, errorNumber);
    this.errorNumber = errorNumber;
    this.errors = new ArrayList<Page>();
  }
  
  /**
   * @return Error number as defined in the check wikipedia project.
   */
  public int getErrorNumber() {
    return errorNumber;
  }

  /**
   * @return Algorithm.
   */
  public CheckErrorAlgorithm getAlgorithm() {
    return algorithm;
  }

  /**
   * @return Number of error pages.
   */
  public int getPageCount() {
    return errors.size();
  }

  /**
   * @param index Page index.
   * @return Error page.
   */
  public Page getPage(int index) {
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
    String count = Integer.toString(errors.size());
    return GT._("Error n°{0} ({1} - {2}) - {3}", new Object[] {
        Integer.valueOf(errorNumber),
        count,
        CheckErrorAlgorithms.getPriorityString(
            (algorithm != null) ? algorithm.getPriority() : CheckErrorAlgorithms.PRIORITY_UNKOWN),
        (algorithm != null) ?
            algorithm.getShortDescriptionReplaced() :
            GT._("Error unkown from WikiCleaner") });
  }

  /**
   * Remove a page from the list of errors.
   * 
   * @param page Page.
   */
  public void remove(Page page) {
    if (page == null) {
      return;
    }
    for (int i = errors.size(); i > 0; i--) {
      if (Page.areSameTitle(page.getTitle(), errors.get(i - 1).getTitle())) {
        errors.remove(i - 1);
      }
    }
  }

  /**
   * Fix an error for the page.
   * 
   * @param page Page.
   * @param errorNumber Error number.
   * @return Flag indicating if fix was done.
   */
  public static boolean fix(Page page, String errorNumber) {
    try {
      NameValuePair[] parameters = new NameValuePair[] {
          new NameValuePair("id", Integer.toString(Integer.parseInt(errorNumber))),
          new NameValuePair("pageid", Integer.toString(page.getPageId())),
          new NameValuePair("project", page.getWikipedia().getCheckWikiCode()),
          new NameValuePair("view", "only")
      };
      APIFactory.getAPI().askToolServerPost(
          "~sk/cgi-bin/checkwiki/checkwiki.cgi", parameters, false);
    } catch (NumberFormatException e) {
      return false;
    } catch (APIException e) {
      return false;
    }
    return true;
  }

  /**
   * Fix an error for the page.
   * 
   * @param page Page.
   * @return Flag indicating if fix was done.
   */
  public boolean fix(Page page) {
    errors.remove(page);
    return fix(page, Integer.toString(errorNumber));
  }
}
