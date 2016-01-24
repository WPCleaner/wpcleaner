/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;

import java.util.ArrayList;
import java.util.List;


/**
 * Configuration for String list attributes.
 */
public enum WPCConfigurationStringList {

  // Possible replacements for abbreviations
  ABBREVIATIONS("general_abbreviations", true, true, true),
  // Comment used for a normal link to a disambiguation page.
  COMMENTS_FOR_DAB_LINK("dab_link_comments", false, true, true),
  // Common languages.
  COMMON_LANGUAGES("common_languages", true, true, true),
  // Pages containing the current list of disambiguation pages.
  CURRENT_DAB_LIST("dab_list", false, true, false),
  // List of templates for displaying a warning when editing.
  EDIT_WARNING_TEMPLATES("general_edit_warning_templates", true, true, true),
  // Texts that can be inserted
  INSERT_TEXTS("general_insert_texts", true, true, true),
  // Categories for ISBN errors.
  ISBN_ERRORS_CATEGORIES("isbn_errors_categories", true, true, true),
  // Lists for ISBN errors.
  ISBN_ERRORS_LISTS("isbn_errors_lists", true, true, true),
  // Templates to ask for help about ISBN.
  ISBN_HELP_NEEDED_TEMPLATES("general_isbn_help_needed_templates", true, true, true),
  // ISBN search engines.
  ISBN_SEARCH_ENGINES("general_isbn_search_engines", true, true, true, new String[] { 
      "WorldCat|http://www.worldcat.org/search?q={0}",
      "ISBNdb|http://isbndb.com/search/all?query={0}",
      "OttoBib|http://www.ottobib.com/isbn/{0}",
      "Copyright Clearance Center|http://www.copyright.com/openurl.do?isbn={0}&servicename=all&WT.mc_id=wikipedia",
  } ),
  // ISBN search engines.
  ISBN_SEARCH_ENGINES_TEMPLATES("general_isbn_search_engines_templates", true, true, true),
  // ISBN special values.
  ISBN_SPECIAL_VALUES("isbn_special_values", true, true, true),
  // Templates for ISBN.
  ISBN_TEMPLATES("general_isbn_templates", true, true, true, new String[] { "ISBN|1" } ),
  // Categories for ISSN errors.
  ISSN_ERRORS_CATEGORIES("issn_errors_categories", true, true, true),
  // Lists for ISSN errors.
  ISSN_ERRORS_LISTS("issn_errors_lists", true, true, true),
  // Templates to ask for help about ISSN.
  ISSN_HELP_NEEDED_TEMPLATES("general_issn_help_needed_templates", true, true, true),
  // Templates to ignore for ISSN.
  ISSN_IGNORE_TEMPLATES("issn_ignore_templates", true, true, true),
  // ISSN search engines.
  ISSN_SEARCH_ENGINES("general_issn_search_engines", true, true, true, new String[] { 
      "WorldCat|http://worldcat.org/issn/{0}",
      "MIAR|http://miar.ub.edu/issn/{0}",
  } ),
  // ISSN search engines.
  ISSN_SEARCH_ENGINES_TEMPLATES("general_issn_search_engines_templates", true, true, true),
  // ISSN special values.
  ISSN_SPECIAL_VALUES("issn_special_values", true, true, true),
  // Templates for ISSN.
  ISSN_TEMPLATES("general_issn_templates", true, true, true, new String[] { "ISSN|1" } ),
  // Pages containing the list of pages with many disambiguation links.
  MOST_DAB_LINKS("most_dab_links", false, true, false),
  // List of templates for preventing bot editions.
  NOBOT_TEMPLATES("general_nobot_templates", true, true, true),
  // Templates to ask for help about PMID.
  PMID_HELP_NEEDED_TEMPLATES("general_pmid_help_needed_templates", true, true, true),
  // Templates for PMID.
  PMID_TEMPLATES("general_pmid_templates", true, true, true, null ),
  // Categories that can be applied to redirect page.
  REDIRECT_CATEGORIES("redirect_categories", true, true, true),
  // Templates that can be applied to redirect page.
  REDIRECT_TEMPLATES("redirect_templates", true, true, true),
  // Templates that can be used instead of <references/>.
  REFERENCES_TEMPLATES("general_references_templates", true, true, true),
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
  // Templates ignored for disambiguation link
  TEMPLATES_IGNORE_DAB("dab_ignore_templates", true, true, true),
  // Templates creating links to "to do" lists.
  TODO_LINK_TEMPLATES("general_todo_link_templates", false, true, false),
  // Templates creating "to do" lists.
  TODO_TEMPLATES("general_todo_templates", false, true, false),
  // List of templates that should be before the warning.
  WARNING_AFTER_TEMPLATES("warning_after_templates", false, true, false);

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
   * Default value.
   */
  private final List<String> defaultValue;

  /**
   * @param attributeName Attribute name.
   * @param canCombine True if general attribute and user attribute can be combined.
   * @param generalAttribute True if it can be set as a general attribute.
   * @param userAttribute True if it can be set as a user attribute.
   */
  WPCConfigurationStringList(
      String attributeName, boolean canCombine,
      boolean generalAttribute, boolean userAttribute) {
    this(attributeName, canCombine, generalAttribute, userAttribute, null);
  }

  /**
   * @param attributeName Attribute name.
   * @param canCombine True if general attribute and user attribute can be combined.
   * @param generalAttribute True if it can be set as a general attribute.
   * @param userAttribute True if it can be set as a user attribute.
   * @param defaultValue Default value.
   */
  WPCConfigurationStringList(
      String attributeName, boolean canCombine,
      boolean generalAttribute, boolean userAttribute,
      String[] defaultValue) {
    this.attributeName = attributeName;
    this.canCombine = canCombine;
    this.generalAttribute = generalAttribute;
    this.userAttribute = userAttribute;
    if (defaultValue != null) {
      this.defaultValue = new ArrayList<String>(defaultValue.length);
      for (String element : defaultValue) {
        this.defaultValue.add(element);
      }
    } else {
      this.defaultValue = null;
    }
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

  /**
   * @return Default value.
   */
  public List<String> getDefaultValue() {
    return defaultValue;
  }
}
