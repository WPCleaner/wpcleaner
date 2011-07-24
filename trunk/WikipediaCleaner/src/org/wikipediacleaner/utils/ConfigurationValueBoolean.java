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
 * Configuration for Boolean attributes.
 */
public enum ConfigurationValueBoolean {

  ADVANCED_FEATURES("AdvancedFeatures", false),
  ANALYSIS_COUNT_DISAMBIG("AnalysisCountDisambiguation", true),
  ANALYSIS_COUNT_MISSING("AnalysisCountMissing", false),
  ANALYSIS_COUNT_OTHER("AnalysisCountOther", false),
  ANALYSIS_COUNT_REDIRECT("AnalysisCountRedirect", false),
  ANALYSIS_DISAMBIG_PAGES("AnalysisDisambiguationPages", true),
  ANALYSIS_HIDE_SENDING("AnalysisHideSending", true),
  ANALYSIS_MISSING_PAGES("AnalysisMissingPages", false),
  ANALYSIS_OTHER_PAGES("AnalysisOtherPages", false),
  ANALYSIS_REDIRECT_PAGES("AnalysisRedirectPages", false),
  CHECK_SHOW_0_ERRORS("CheckShow0Errors", false),
  CHECK_LINK_ERRORS("CheckLinkErrors", false),
  CHECK_MARK_AS_FIXED("CheckMarkAsFixed", false),
  CLOSE_DISAMBIG("CloseDisambiguation", false),
  CLOSE_FULL("CloseFullAnalysis", true),
  CREATE_DAB_WARNING("CreateDabWarning", true),
  CREATE_DAB_WARNING_ALL("CreateDabWarningAll", false),
  CREATE_DAB_WARNING_ENCY("CreateDabWarningEncyclo", true),
  DEBUG_TIME("DebugTime", false),
  DEBUG_URL("DebugURL", true),
  DEBUG_XML("DebugXML", false),
  FORCE_WATCH("ForceWatch", false),
  ORTHOGRAPH("Orthograph", true),
  REMEMBER_LAST_PAGE("RememberLastPage", true),
  RESTORE_WINDOW("RestoreWindow", true),
  SAVE_LAST_REPLACEMENT("SaveLastReplacement", false),
  SAVE_WINDOW("SaveWindow", true),
  SHORT_NOTATION("ShortNotation", false),
  SYNTAX_HIGHLIGHTING("SyntaxHighlighting", false),
  UPDATE_DAB_WARNING("UpdateDabWarning", true),
  UPDATE_DAB_WARNING_ALL("UpdateDabWarningAll", true),
  UPDATE_DAB_WARNING_ENCY("UpdateDabWarningEncyclo", true),
  WIKICLEANER_COMMENT("WikiCleanerComment", true);

  /**
   * Attribute name.
   */
  private final String name;

  /**
   * Attribute default value.
   */
  private final boolean defaultValue;

  /**
   * @param name Attribute name.
   * @param defaultValue Attribute default value.
   */
  ConfigurationValueBoolean(String name, boolean defaultValue) {
    this.name = name;
    this.defaultValue = defaultValue;
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @return Current value of the attribute.
   */
  static boolean getValue(Preferences preferences, ConfigurationValueBoolean attribute) {
    if (attribute == null) {
      return false;
    }
    return attribute.getValue(preferences);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @return Current value of the attribute.
   */
  boolean getValue(Preferences preferences) {
    if (preferences == null) {
      return getDefaultValue();
    }
    return preferences.getBoolean(getName(), getDefaultValue());
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @param value New value of the attribute.
   */
  static void setValue(Preferences preferences, ConfigurationValueBoolean attribute, boolean value) {
    if (attribute == null) {
      return;
    }
    attribute.setValue(preferences, value);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param value New value of the attribute.
   */
  void setValue(Preferences preferences, boolean value) {
    if (preferences == null) {
      return;
    }
    preferences.putBoolean(getName(), value);
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
  public boolean getDefaultValue() {
    return defaultValue;
  }
}
