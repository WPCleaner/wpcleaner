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

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;


/**
 * Abstract base class for wiki configuration.
 */
public abstract class AbstractWikiSettings {

  /**
   * @return Wiki language.
   */
  public abstract String getLanguage();

  /**
   * @return Wiki code.
   */
  public abstract String getCode();

  /**
   * @return CheckWiki code.
   */
  public abstract String getCodeCheckWiki();

  /**
   * @return Wiki name.
   */
  public abstract String getName();

  /**
   * @return Component orientation.
   */
  public ComponentOrientation getComponentOrientation() {
    return ComponentOrientation.LEFT_TO_RIGHT;
  }

  /**
   * @param secured True if secured connection is requested.
   * @return Host URL (URL to wiki host).
   */
  public abstract String getHostURL(boolean secured);

  /**
   * @return API URL (URL to api.php).
   */
  public abstract String getApiURL();

  /**
   * @param secured True if secured connection is requested.
   * @return Index URL (URL to index.php).
   */
  public abstract String getIndexURL(boolean secured);

  /**
   * @param pageTitle Title.
   * @param redirect Follow redirect ?
   * @param secured True if secured connection is requested.
   * @return URL of the wiki.
   */
  public String getURL(String pageTitle, boolean redirect, boolean secured) {
    try {
      return getIndexURL(secured) +
             "?title=" + URLEncoder.encode(pageTitle, "UTF-8") +
             (redirect ? "" : "&redirect=no");
    } catch (UnsupportedEncodingException e) {
      return getIndexURL(secured);
    }
  }

  /**
   * @param pageTitle Title.
   * @param action Action.
   * @param secured True if secured connection is requested.
   * @return URL of the wiki.
   */
  public String getURL(String pageTitle, String action, boolean secured) {
    try {
      return getIndexURL(secured) +
             "?title=" + URLEncoder.encode(pageTitle, "UTF-8") +
             "&redirect=no" +
             "&action=" + action;
    } catch (UnsupportedEncodingException e) {
      return getIndexURL(secured);
    }
  }

  /**
   * @return Configuration page.
   */
  public String getConfigurationPage() {
    return "WikiCleanerConfiguration";
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return getCode() + " - " + getName();
  }
}
