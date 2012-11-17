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
 * Configuration for String list attributes.
 */
public enum WPCConfigurationStringList {

  // Pages containing the current list of disambiguation pages.
  CURRENT_DAB_LIST("dab_list", false, true, false),
  // List of templates that should be before the disambiguation warning.
  DAB_WARNING_AFTER_TEMPLATES("dab_warning_after_templates", false, true, false),
  // Pages containing the list of pages with many disambiguation links.
  MOST_DAB_LINKS("most_dab_links", false, true, false),
  // Chapters to be ignored for suggestions.
  SUGGESTION_IGNORE("general_suggestions_ignore", true, true, true),
  // Pages containing spelling suggestions.
  SUGGESTION_PAGES("general_suggestions", true, true, true),
  // Pages containing spelling suggestions in AWB format.
  SUGGESTION_TYPO_PAGES("general_suggestions_typo", true, true, true),
  // Templates used after a disambiguation link asking for help.
  TEMPLATES_AFTER_HELP_ASKED("dab_help_asked_templates_after", false, true, false),
  // Templates used for a normal link to a disambiguation page.
  TEMPLATES_FOR_DAB_LINK("dab_link_templates", false, true, false),
  // Templates used for finding pages where help is requested.
  TEMPLATES_FOR_HELP_REQUESTED("help_requested_templates", false, true, false),
  // Templates used for linking text.
  TEMPLATES_FOR_LINKING_TEXT("link_text_templates", false, true, false),
  // Templates used for a link where help is required.
  TEMPLATES_FOR_NEEDING_HELP("needing_help_templates", false, true, false),
  // Templates creating links to "to do" lists.
  TODO_LINK_TEMPLATES("general_todo_link_templates", false, true, false),
  // Templates creating "to do" lists.
  TODO_TEMPLATES("general_todo_templates", false, true, false);

  /**
   * Attribute name.
   */
  private final String attributeName;

  /**
   * True if general attribute and user attribute can be combined.
   */
  private final boolean canCombine;

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
   * @param canCombine True if general attribute and user attribute can be combined.
   * @param generalAttribute True if it can be set as a general attribute.
   * @param userAttribute True if it can be set as a user attribute.
   */
  WPCConfigurationStringList(
      String attributeName, boolean canCombine,
      boolean generalAttribute, boolean userAttribute) {
    this.attributeName = attributeName;
    this.canCombine = canCombine;
    this.generalAttribute = generalAttribute;
    this.userAttribute = userAttribute;
  }

  /**
   * Find attribute by its name.
   * 
   * @param attributeName Attribute name.
   * @return Attribute for the given name.
   */
  public static WPCConfigurationStringList getValue(String attributeName) {
    if (attributeName == null) {
      return null;
    }
    attributeName = attributeName.trim();
    for (WPCConfigurationStringList value : values()) {
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
   * @return True if general attribute and user attribute can be combined.
   */
  public boolean canCombine() {
    return canCombine;
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
