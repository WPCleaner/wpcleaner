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

import java.util.prefs.Preferences;


/**
 * Configuration for String attributes.
 */
public enum ConfigurationValueString {

  LAST_DAB_WARNING("LastDabWarning", null),
  PAGE_NAME("PageName", null),
  PASSWORD("Password", null),
  PLAF_NAME("LookAndFeelName", "Metal"),
  SIGNATURE("Signature", "--~~~~"),
  USER_NAME("UserName", null);

  /**
   * Attribute name.
   */
  private final String name;

  /**
   * Attribute default value.
   */
  private final String defaultValue;

  /**
   * @param name Attribute name.
   * @param defaultValue Attribute default value.
   */
  ConfigurationValueString(String name, String defaultValue) {
    this.name = name;
    this.defaultValue = defaultValue;
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @return Current value of the attribute.
   */
  static String getValue(Preferences preferences, ConfigurationValueString attribute) {
    if (attribute == null) {
      return null;
    }
    return attribute.getValue(preferences);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @return Current value of the attribute.
   */
  String getValue(Preferences preferences) {
    if (preferences == null) {
      return getDefaultValue();
    }
    return preferences.get(getName(), getDefaultValue());
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @param value New value of the attribute.
   */
  static void setValue(Preferences preferences, ConfigurationValueString attribute, String value) {
    if (attribute == null) {
      return;
    }
    attribute.setValue(preferences, value);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param value New value of the attribute.
   */
  void setValue(Preferences preferences, String value) {
    if (preferences == null) {
      return;
    }
    if (value != null) {
      preferences.put(getName(), value);
    } else {
      preferences.remove(getName());
    }
  }

  /**
   * @return Name of the configuration attribute.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Default value of the configuration attribute.
   */
  public String getDefaultValue() {
    return defaultValue;
  }
}
