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

package org.wikipediacleaner.api.constants;

import java.text.DecimalFormat;
import java.util.Properties;

import org.wikipediacleaner.i18n.GT;


/**
 * Configuration for Check Wiki project.
 */
public class CWConfiguration {

  /**
   * Check Wiki project code.
   */
  private final String code;

  public CWConfiguration(String code) {
    this.code = code;
  }

  /**
   * Page of the Check Wiki projet.
   */
  private String projectPage;

  /**
   * @return Page of the Check Wiki project.
   */
  public String getProjectPage() {
    return projectPage;
  }

  /**
   * @param projectPage Page of the Check Wiki project.
   */
  public void setProjectPage(String projectPage) {
    this.projectPage = null;
    if ((projectPage != null) && (projectPage.trim().length() > 0)) {
      this.projectPage = projectPage.trim();
    }
  }

  /**
   * Comment for Check Wiki project.
   */
  private String comment;

  /**
   * @return Comment for Check Wiki project.
   */
  public String getComment() {
    if (comment != null) {
      return comment;
    }
    return GT._("Fixed using [[{0}]]", getProjectPage());
  }

  /**
   * @param comment Comment for Check Wiki project.
   */
  public void setComment(String comment) {
    this.comment = null;
    if ((comment != null) && (comment.trim().length() > 0)) {
      this.comment = comment.trim();
    }
  }

  /**
   * Translation page for Check Wiki project.
   */
  private String translationPage;

  /**
   * @return Translation page for Check Wiki project.
   */
  public String getTranslationPage() {
    return translationPage;
  }

  /**
   * @param page Translation page for Check Wiki project.
   */
  public void setTranslationPage(String page) {
    translationPage = null;
    if ((page != null) && (page.trim().length() > 0)) {
      this.translationPage = page;
    }
  }

  // TODO: Group configuration by error number (analyze each property name when setting properties)

  /**
   * General Check Wiki project configuration.
   */
  private Properties generalConfiguration;

  /**
   * @param configuration General Check Wiki project configuration.
   */
  public void setGeneralConfiguration(Properties configuration) {
    generalConfiguration = configuration;
  }

  /**
   * Check Wiki project configuration specific for this wiki.
   */
  private Properties wikiConfiguration;

  /**
   * @param configuration Check Wiki project configuration specific for this wiki.
   */
  public void setWikiConfiguration(Properties configuration) {
    wikiConfiguration = configuration;
  }

  /**
   * Format for error numbers.
   */
  private final DecimalFormat errorNumberFormat = new DecimalFormat("000");

  public String getProperty(
      String propertyName, int errorNumber,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty) {
    String errorPrefix;
    synchronized (errorNumberFormat) {
      errorPrefix = "error_" + errorNumberFormat.format(errorNumber) + "_" + propertyName + "_";
    }
    String result = null;
    if ((useWiki) && (wikiConfiguration != null)) {
      result = wikiConfiguration.getProperty(errorPrefix + code, null);
    }
    if ((result != null) && ((acceptEmpty) || (result.trim().length() > 0))) {
      return result.trim();
    }
    if ((useGeneral) && (generalConfiguration != null)) {
      result = generalConfiguration.getProperty(errorPrefix + "script", null);
    }
    if (result != null) {
      return result.trim();
    }
    return null;
  }
}
