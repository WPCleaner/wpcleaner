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

import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.Properties;

import org.wikipediacleaner.i18n.GT;


/**
 * Configuration for Check Wiki project.
 */
public class CWConfiguration {

  /**
   * Maximum number for error numbers.
   */
  public static final int MAX_ERROR_NUMBER = 999;

  /**
   * Check Wiki project code.
   */
  private final String code;

  public CWConfiguration(String code) {
    this.code = code;
    this.configuration = new ArrayList<CWConfigurationError>();
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

  /**
   * Configuration for each error.
   */
  private final ArrayList<CWConfigurationError> configuration;

  /**
   * Prefix for property names.
   */
  private final static String PREFIX = "error_";

  /**
   * Set configuration.
   * 
   * @param properties Properties read from configuration file.
   * @param suffix Suffix for properties to consider.
   * @param otherSuffix Suffix for properties not to consider.
   * @param general Flag indicating if dealing with general properties or wiki properties.
   */
  private void setConfiguration(
      Properties properties,
      String suffix, String otherSuffix,
      boolean general) {

    // Clean previous configuration
    for (CWConfigurationError error : configuration) {
      if (error != null) {
        if (general) {
          error.clearGeneralConfiguration();
        } else {
          error.clearWikiConfiguration();
        }
      }
    }
    if ((properties == null) || (suffix == null) || (otherSuffix == null)) {
      return;
    }

    // Check every property
    for (Entry<Object, Object> property : properties.entrySet()) {
      String name = property.getKey().toString();
      String value = property.getValue().toString();
      boolean matches = true;

      // Check that property prefix is OK
      if (matches && name.startsWith(PREFIX)) {
        name = name.substring(PREFIX.length());
      } else {
        matches = false;
      }

      // Check that property suffix is OK
      boolean goodSuffix = false;
      if (matches && name.endsWith(suffix)) {
        name = name.substring(0, name.length() - suffix.length());
        goodSuffix = true;
      } else if (matches && name.endsWith(otherSuffix)) {
        name = name.substring(0, name.length() - otherSuffix.length());
      } else {
        matches = false;
      }

      // Retrieve error number
      int errorNumber = -1;
      if (matches && (name.length() > 4) &&
          Character.isDigit(name.charAt(0)) &&
          Character.isDigit(name.charAt(1)) &&
          Character.isDigit(name.charAt(2)) &&
          (name.charAt(3) == '_')) {
        try {
          errorNumber = Integer.parseInt(name.substring(0, 3));
        } catch (NumberFormatException e) {
          // Not supposed to happen
        }
        name = name.substring(4);
      } else {
        matches = false;
      }

      // Save property
      if (matches && (errorNumber > 0) && (errorNumber <= MAX_ERROR_NUMBER)) {
        if (goodSuffix) {
          CWConfigurationError error = null;
          if (configuration.size() > errorNumber) {
            error = configuration.get(errorNumber);
          }
          if (error == null) {
            error = new CWConfigurationError(errorNumber);
            configuration.ensureCapacity(errorNumber + 1);
            while (configuration.size() < errorNumber + 1) {
              configuration.add(configuration.size(), null);
            }
            configuration.set(errorNumber, error);
          }
          error.addProperty(name, value, general);
        }
      }
    }
  }

  /**
   * @param configuration General Check Wiki project configuration.
   */
  public void setGeneralConfiguration(Properties configuration) {
    setConfiguration(configuration, "_script", "_" + code, true);
  }

  /**
   * @param configuration Check Wiki project configuration specific for this wiki.
   */
  public void setWikiConfiguration(Properties configuration) {
    setConfiguration(configuration, "_" + code, "_script", false);
  }

  /**
   * @param errorNumber Error number.
   * @return Configuration for error.
   */
  public CWConfigurationError getErrorConfiguration(int errorNumber) {
    if ((errorNumber < 0) || (errorNumber >= configuration.size())) {
      return null;
    }
    return configuration.get(errorNumber);
  }

  /**
   * @param propertyName Property name.
   * @param errorNumber Error number.
   * @param useWiki Flag indicating if wiki configuration can be used.
   * @param useGeneral Flag indicating if general configuration can be used.
   * @param acceptEmpty Flag indicating if empty strings are accepted.
   * @return
   */
  /*public String getProperty(
      String propertyName, int errorNumber,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty) {
    if (propertyName == null) {
      return null;
    }
    if ((errorNumber < 0) || (errorNumber >= configuration.size())) {
      return null;
    }
    CWConfigurationError error = configuration.get(errorNumber);
    if (error == null) {
      return null;
    }
    return error.getProperty(propertyName, useWiki, useGeneral, acceptEmpty);
  }*/
}
