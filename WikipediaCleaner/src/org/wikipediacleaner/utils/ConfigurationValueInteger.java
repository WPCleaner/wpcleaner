/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.util.prefs.Preferences;


/**
 * Configuration for Integer attributes.
 */
public enum ConfigurationValueInteger {

  ANALYSIS_NB_PAGES("AnalysisNbPages", 10),
  ANALYSIS_UNDO_LVL("AnalysisUndoLevels", 10),
  CHECK_BOT_NB_PAGES("CheckBotNbPages", 100),
  CHECK_NB_ERRORS("CheckNbErrors", 100),
  CONFIG_VERSION("ConfigurationVersion", 1),
  EDITOR_FONT_SIZE("EditorFontSize", 12),
  FONT_SIZE("FontSize", 0),
  INTERROG_THREAD("InterrogationThreads", 30),
  MAXIMUM_PAGES("MaximumPages", 20),
  MAX_CATEGORY_MEMBERS("MaxCategoryMembers", 1000),
  MAX_DAYS_ABUSE_LOG("MaxDaysAbuseLog", 7),
  MAX_EDITS_PER_MINUTE("MaxEditsPerMinute", 0),
  MAX_EMBEDDED_IN("MaxEmbeddedIn", 1000),
  MAX_PAGES_WITH_PROP("MaxPagesWithProp", 1000),
  MAX_PROTECTED_TITLES("MaxProtectedTitles", 1000),
  MAX_QUERY_PAGE("MaxQueryPage", 1000),
  MAX_SEARCH("MaxSearch", 1000),
  MENU_SIZE("MenuSize", 30),
  PLAF_TYPE("LookAndFeelType", ConfigurationConstants.VALUE_PLAF_TYPE_WPCLEANER),
  SAVE_USER("SaveUser", ConfigurationConstants.VALUE_SAVE_USER_NAME),
  SLOW_REGEXP("SlowRegexp", 1000),
  SYNTAX_HIGHLIGHTING_LIMIT("SyntaxHighlightingLimit", 40000),
  TIME_BETWEEN_EDIT("TimeBetweenEdit", 0);

  public static final int MAX_EDITS_PER_MINUTE_NORMAL = 4;

  /**
   * Attribute name.
   */
  private final String name;

  /**
   * Attribute default value.
   */
  private final int defaultValue;

  /**
   * @param name Attribute name.
   * @param defaultValue Attribute default value.
   */
  ConfigurationValueInteger(String name, int defaultValue) {
    this.name = name;
    this.defaultValue = defaultValue;
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @return Current value of the attribute.
   */
  static int getValue(Preferences preferences, ConfigurationValueInteger attribute) {
    if (attribute == null) {
      return 0;
    }
    return attribute.getValue(preferences);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @return Current value of the attribute.
   */
  int getValue(Preferences preferences) {
    if (preferences == null) {
      return getDefaultValue();
    }
    return preferences.getInt(getName(), getDefaultValue());
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @param value New value of the attribute.
   */
  static void setValue(Preferences preferences, ConfigurationValueInteger attribute, int value) {
    if (attribute == null) {
      return;
    }
    attribute.setValue(preferences, value);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param value New value of the attribute.
   */
  void setValue(Preferences preferences, int value) {
    if (preferences == null) {
      return;
    }
    preferences.putInt(getName(), value);
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
  public int getDefaultValue() {
    return defaultValue;
  }
}
