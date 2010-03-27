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
import java.util.regex.Pattern;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.methods.PostMethod;
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
        line = line.replaceAll(Pattern.quote("&#039;"), "'");
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
    return GT._("Error n°{0} ({1}) - {2}", new Object[] {
        Integer.valueOf(errorNumber),
        count,
        (algorithm != null) ?
            algorithm.getErrorDescription() :
            GT._("Error unkown from WikiCleaner") });
  }

  public boolean fix(Page page) {
    try {
      MultiThreadedHttpConnectionManager manager = new MultiThreadedHttpConnectionManager();
      HttpClient httpClient = new HttpClient(manager);
      String url = "http://toolserver.org/~sk/cgi-bin/checkwiki/checkwiki.cgi";
      PostMethod method = new PostMethod(url);
      method.getParams().setContentCharset("UTF-8");
      method.setRequestHeader("Accept-Encoding", "gzip");
      method.addParameter("id", Integer.toString(errorNumber));
      method.addParameter("pageid", Integer.toString(page.getPageId()));
      method.addParameter("project", wikipedia.getCode() + "wiki");
      method.addParameter("view", "only");
      int statusCode = httpClient.executeMethod(method);
      if (statusCode != HttpStatus.SC_OK) {
        return false;
      }
      
      this.errors.remove(page);
    } catch (HttpException e) {
      return false;
    } catch (IOException e) {
      return false;
    }
    return true;
  }
}
