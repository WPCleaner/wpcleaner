/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;


/**
 * A text provider for URL titles.
 */
public class TextProviderUrlTitle implements TextProvider {

  /**
   * URL. 
   */
  private final String url;

  /**
   * Maximum size of URL contents read to find title.
   */
  private final static int MAXIMUM_SIZE = 10000;

  /**
   * @param url URL.
   */
  public TextProviderUrlTitle(String url) {
    this.url = url;
  }

  /**
   * @return Possible texts.
   */
  public Collection<String> getTexts() {
    Collection<String> result = new ArrayList<String>();
    if (url != null) {
      GetMethod method = null;
      InputStream is = null;
      try {
        HttpClient httpClient = new HttpClient();
        method = new GetMethod(url);
        System.out.println(url);
        int statusCode = httpClient.executeMethod(method);
        if (statusCode == HttpStatus.SC_OK) {
          is = method.getResponseBodyAsStream();
          byte[] tmpBytes = new byte[MAXIMUM_SIZE];
          int size = is.read(tmpBytes);
          String text = new String(tmpBytes, 0, size).replaceAll("\\s", " ");
          Pattern[] pTitles = {
              Pattern.compile("<title>(.+?)</title>", Pattern.CASE_INSENSITIVE),
              Pattern.compile("<meta name=\"title\" content=\"([^\"]+?)\"/?>", Pattern.CASE_INSENSITIVE),
              Pattern.compile("<meta name=\"description\" content=\"([^\"]+?)\"/?>", Pattern.CASE_INSENSITIVE)
          };
          for (Pattern pTitle : pTitles) {
            Matcher m = pTitle.matcher(text);
            if (m.find() == true) {
              String title = m.group(1).trim();
              title = title.replaceAll("&#39;", "’");
              title = title.replaceAll("&#232;", "è");
              title = title.replaceAll("&#233;", "é");
              title = title.replaceAll("&eacute;", "é");
              result.add(title);
            }
          }
        }
      } catch (IOException ex) {
        // Nothing to do
      } catch (Exception ex) {
        // Nothing to do
      } finally {
        if (is != null) {
          try {
            is.close();
          } catch (IOException ex) {
            // Nothing to do 
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
    }
    return result;
  }

}
