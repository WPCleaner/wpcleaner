/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;


/**
 * Configuration for Boolean attributes.
 */
public enum WPCConfigurationBoolean {

  // Used to force the activation of Check Wiki project even without a project page.
  CW_FORCE("check_wiki_force", false, true, false),
  // Allow use of __DISAMBIG__ magic word.
  DAB_USE_DISAMBIG_MAGIC_WORD("dab_use_disambig", true, true, false),
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
