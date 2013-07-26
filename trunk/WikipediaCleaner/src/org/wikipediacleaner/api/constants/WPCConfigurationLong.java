/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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


/**
 * Configuration for Long attributes.
 */
public enum WPCConfigurationLong {

  // Delay for continuing monitoring of articles
  RC_KEEP_MONITORING_DELAY("rc_keep_monitoring_delay", 120, true, true),
  // Delay before checking disambiguation links in new articles
  RC_NEW_ARTICLE_WITH_DAB_DELAY("rc_new_article_with_dab_delay", 15, true, true);

  /**
   * Attribute name.
   */
  private final String attributeName;

  /**
   * Default value for Long attribute.
   */
  private final long defaultValue;

  /**
   * True if it can be set as a general attribute.
   */
  private final boolean generalAttribute;

  /**
   * True if it can be set as a user attribute.
   */
  private final boolean userAttribute;

  /**
   * @param attributeName Attribute name.
   * @param defaultValue Default value for Long attribute.
   * @param generalAttribute True if it can be set as a general attribute.
   * @param userAttribute True if it can be set as a user attribute.
   */
  WPCConfigurationLong(
      String attributeName, long defaultValue,
      boolean generalAttribute, boolean userAttribute) {
    this.attributeName = attributeName;
    this.defaultValue = defaultValue;
    this.generalAttribute = generalAttribute;
    this.userAttribute = userAttribute;
  }

  /**
   * Find attribute by its name.
   * 
   * @param attributeName Attribute name.
   * @return Attribute for the given name.
   */
  public static WPCConfigurationLong getValue(String attributeName) {
    if (attributeName == null) {
      return null;
    }
    attributeName = attributeName.trim();
    for (WPCConfigurationLong value : values()) {
      if (attributeName.equals(value.getAttributeName())) {
        return value;
      }
    }
    return null;
  }

  /**
   * @return Attribute name.
   */
  public String getAttributeName() {
    return attributeName;
  }

  /**
   * @return Default value for Long attribute.
   */
  public long getDefaultValue() {
    return defaultValue;
  }

  /**
   * @return True if it can be set as a general attribute.
   */
  public boolean isGeneralAttribute() {
    return generalAttribute;
  }

  /**
   * @return True if it can be set as a user attribute.
   */
  public boolean isUserAttribute() {
    return userAttribute;
  }
}
