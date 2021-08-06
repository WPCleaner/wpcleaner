/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.util.prefs.Preferences;


/**
 * Configuration for String attributes.
 */
public enum ConfigurationValueString {

  ACT_AS_USER("ActAsUser", null),
  COMMENT("Comment", null),
  EDITOR_FONT_NAME("EditorFontName", "SansSerif.plain"),
  LAST_DAB_WARNING("LastDabWarning", null),
  LAST_DUMP_FILE("LastDumpFile", null),
  LAST_DUPLICATE_ARGS_WARNING("LastDuplicateArgsWarning", null),
  LAST_EXPORT_DIRECTORY("LastExportDirectory", null),
  LAST_EXPORT_PAGE("LastExportPage", null),
  LAST_ISBN_WARNING("LastISBNWarning", null),
  LAST_ISSN_WARNING("LastISSNWarning", null),
  LAST_LANGUAGE("LastLanguage", null),
  LAST_REPLACEMENTS_DIRECTORY("LastReplacementsDirectory", null),
  LAST_UNKNOWN_PARAMETER_WARNING("LastUnknownParameterWarning", null),
  LAST_USER("LastUser", null),
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
