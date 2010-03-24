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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.methods.PostMethod;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.i18n.GT;

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
    MultiThreadedHttpConnectionManager manager = new MultiThreadedHttpConnectionManager();
    HttpClient httpClient = new HttpClient(manager);
    PostMethod method = null;
    for (int errorNumber = 1; errorNumber < 100; errorNumber++) {
      String className = CheckErrorAlgorithm.class.getName() + Integer.toString(errorNumber);
      try {
        setText(GT._("Checking for errors number {0}", Integer.toString(errorNumber)));

        // Checking if the error number is known by WikiCleaner
        Class.forName(className);

        // Retrieving list of pages for the error number
        String url = "http://toolserver.org/~sk/cgi-bin/checkwiki/checkwiki.cgi";
        method = new PostMethod(url);
        method.getParams().setContentCharset("UTF-8");
        method.setRequestHeader("Accept-Encoding", "gzip");
        method.addParameter("id", Integer.toString(errorNumber));
        method.addParameter("limit", Integer.toString(500));
        method.addParameter("offset", Integer.toString(0));
        method.addParameter("project", getWikipedia().getCode() + "wiki");
        method.addParameter("view", "bots");
        int statusCode = httpClient.executeMethod(method);
        if (statusCode != HttpStatus.SC_OK) {
          return new APIException("URL access returned " + HttpStatus.getStatusText(statusCode));
        }
        InputStream stream = method.getResponseBodyAsStream();
        stream = new BufferedInputStream(stream);
        Header contentEncoding = method.getResponseHeader("Content-Encoding");
        if (contentEncoding != null) {
          if (contentEncoding.getValue().equals("gzip")) {
            stream = new GZIPInputStream(stream);
          }
        }
        CheckError.addCheckError(errors, getWikipedia(), errorNumber, stream);
      } catch (ClassNotFoundException e) {
        // Not found: error not yet available in WikiCleaner.
      } catch (HttpException e) {
        return e;
      } catch (IOException e) {
        return e;
      }
    }
    return null;
  }
}