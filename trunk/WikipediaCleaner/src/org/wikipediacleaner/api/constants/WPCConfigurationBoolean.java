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
 * Configuration for Boolean attributes.
 */
public enum WPCConfigurationBoolean {

  // Force use of disambiguation templates list in MediaWiki:Disambiguationspage.
  DAB_USE_TEMPLATES_LIST("dab_use_templates_list", false, true, false),
  // Allow disambiguation warning outside section 0 of talk page.
  DAB_WARNING_SECTION_0("dab_warning_section_0", true, true, false),
  // Force usage of "to do" sub-page in main name space.
  TODO_SUBPAGE_FORCE("general_todo_subpage_force", false, true, false),
  // Force usage of "to do" sub-page in other name spaces.
  TODO_SUBPAGE_FORCE_OTHER("general_todo_subpage_force_other", false, true, false);

  /**
   * Attribute name.
   */
  private final String attributeName;

  /**
   * Default value for boolean attribute.
   */
  private final boolean defaultValue;

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
   * @param defaultValue Default value for Boolean attribute.
   * @param generalAttribute True if it can be set as a general attribute.
   * @param userAttribute True if it can be set as a user attribute.
   */
  WPCConfigurationBoolean(
      String attributeName, boolean defaultValue,
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
  public static WPCConfigurationBoolean getValue(String attributeName) {
    if (attributeName == null) {
      return null;
    }
    attributeName = attributeName.trim();
    for (WPCConfigurationBoolean value : values()) {
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
   * @return Default value for Boolean attribute.
   */
  public boolean getDefaultValue() {
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
