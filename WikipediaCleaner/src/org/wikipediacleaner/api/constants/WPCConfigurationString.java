/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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
 * Configuration for String attributes.
 */
public enum WPCConfigurationString {

  // Comment for disambiguation links that have been fixed.
  DAB_COMMENT("dab_comment", null, false, true, true),
  // Comment for one disambiguation link that has been fixed.
  DAB_COMMENT_1("dab_comment_1", null, false, true, true),
  // Comment for disambiguation links that still need to be fixed.
  DAB_COMMENT_TODO("dab_comment_todo", ", {0} to be fixed", false, true, true),
  // Comment for one disambiguation link that still need to be fixed.
  DAB_COMMENT_TODO_1("dab_comment_todo_1", null, false, true, true),
  // Comment for warning about disambiguation links in a page.
  DAB_WARNING_COMMENT("dab_warning_comment", null, false, true, true),
  // Comment for warning about one disambiguation link in a page.
  DAB_WARNING_COMMENT_1("dab_warning_comment_1", null, false, true, true),
  // Comment for telling that disambiguation links have been fixed.
  DAB_WARNING_COMMENT_DONE("dab_warning_comment_done", null, false, true, true),
  // Template for warning about disambiguation links in a page.
  DAB_WARNING_TEMPLATE("dab_warning_template", null, false, true, false),
  // Comment for warning template about disambiguation links in a page.
  DAB_WARNING_TEMPLATE_COMMENT("dab_warning_template_comment", null, false, true, false),
  // Help page.
  HELP_PAGE("help_page", null, false, true, false),
  // URL of the help page.
  HELP_URL("help_url", "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner", false, true, false),
  // Template creating a "|".
  PIPE_TEMPLATE("general_pipe_template", null, false, true, false),
  // "To do" sub-page.
  TODO_SUBPAGE("general_todo_subpage", null, false, true, false),
  // Comment used when translating.
  TRANSLATION_COMMENT("translation_comment", null, false, true, true),
  // Wiktionary interwiki.
  WIKTIONARY_INTERWIKI("wikt_interwiki", null, false, true, false);

  /**
   * Attribute name.
   */
  private final String attributeName;

  /**
   * Default value for String attribute.
   */
  private final String defaultValue;

  /**
   * True if value can be empty.
   */
  private final boolean canBeEmpty;

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
   * @param defaultValue Default value for String attribute.
   * @param canBeEmpty True if value can be empty.
   * @param generalAttribute True if it can be set as a general attribute.
   * @param userAttribute True if it can be set as a user attribute.
   */
  WPCConfigurationString(
      String attributeName, String defaultValue, boolean canBeEmpty,
      boolean generalAttribute, boolean userAttribute) {
    this.attributeName = attributeName;
    this.defaultValue = defaultValue;
    this.canBeEmpty = canBeEmpty;
    this.generalAttribute = generalAttribute;
    this.userAttribute = userAttribute;
  }

  /**
   * Find attribute by its name.
   * 
   * @param attributeName Attribute name.
   * @return Attribute for the given name.
   */
  public static WPCConfigurationString getValue(String attributeName) {
    if (attributeName == null) {
      return null;
    }
    attributeName = attributeName.trim();
    for (WPCConfigurationString value : values()) {
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
   * @return Default value for String attribute.
   */
  public String getDefaultValue() {
    return defaultValue;
  }

  /**
   * @return True if value can be empty.
   */
  public boolean canBeEmpty() {
    return canBeEmpty;
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
