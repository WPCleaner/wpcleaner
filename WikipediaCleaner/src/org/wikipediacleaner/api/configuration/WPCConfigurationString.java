/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.configuration;


/**
 * Configuration for String attributes.
 */
public enum WPCConfigurationString {

  // Template for replacing an apostrophe
  APOSTROPHE_TEMPLATE("general_apostrophe_template", null, false, true, true),
  // Automatic formatting: number of carriage returns before categories
  AUTO_CR_BEFORE_CATEGORY("auto_cr_before_category", null, false, true, true),
  // Automatic formatting: number of carriage returns between categories
  AUTO_CR_BETWEEN_CATEGORY("auto_cr_between_category", null, false, true, true),
  // Automatic formatting: number of carriage returns between default sort and categories
  AUTO_CR_DEFAULTSORT_CATEGORY("auto_cr_defaultsort_category", null, false, true, true),
  // Automatic formatting: number of space characters around titles
  AUTO_SPACE_AROUND_TITLE("auto_space_around_title", null, false, true, true),
  // Check Wiki comment
  CW_COMMENT("check_wiki_comment", null, false, true, false),
  // Check Wiki project page
  CW_PROJECT_PAGE("check_wiki_project_page", null, false, true, false),
  // Check Wiki translation page
  CW_TRANSLATION_PAGE("check_wiki_translation_page", null, false, true, false),
  // Comment used when cleaning white list
  CW_WHITELISTE_COMMENT("check_wiki_whitelist_comment", null, false, true, true),
  // Comment for disambiguation links that have been fixed
  DAB_COMMENT("dab_comment", null, false, true, true),
  // Comment for one disambiguation link that has been fixed
  DAB_COMMENT_1("dab_comment_1", null, false, true, true),
  // Comment for disambiguation links for which help has been requested
  DAB_COMMENT_HELP("dab_comment_help", null, false, true, true),
  // Comment for one disambiguation link for which help has been requested
  DAB_COMMENT_HELP_1("dab_comment_help_1", null, false, true, true),
  // Comment for disambiguation links that still need to be fixed
  DAB_COMMENT_TODO("dab_comment_todo", ", {0} to be fixed", false, true, true),
  // Comment for one disambiguation link that still need to be fixed
  DAB_COMMENT_TODO_1("dab_comment_todo_1", null, false, true, true),
  // Template for listing disambiguation links in a page that are normal
  DAB_OK_TEMPLATE("dab_ok_template", null, false, true, true),
  // Comment for warning about disambiguation links in a page
  DAB_WARNING_COMMENT("dab_warning_comment", null, false, true, true),
  // Comment for warning about one disambiguation link in a page
  DAB_WARNING_COMMENT_1("dab_warning_comment_1", null, false, true, true),
  // Comment for telling that disambiguation links have been fixed
  DAB_WARNING_COMMENT_DONE("dab_warning_comment_done", null, false, true, true),
  // Template for warning about disambiguation links in a page
  DAB_WARNING_TEMPLATE("dab_warning_template", null, false, true, false),
  // Comment for warning template about disambiguation links in a page
  DAB_WARNING_TEMPLATE_COMMENT("dab_warning_template_comment", null, false, true, false),
  // localized DEFAULTSORT
  DEFAULTSORT("general_defaultsort", null, false, true, true),
  // Comment for warning about duplicate arguments errors in a page
  DUPLICATE_ARGS_WARNING_COMMENT("duplicate_args_warning_comment", null, false, true, true),
  // Comment for warning about one duplicate arguments error in a page
  DUPLICATE_ARGS_WARNING_COMMENT_1("duplicate_args_warning_comment_1", null, false, true, true),
  // Comment for telling that duplicate arguments errors have been fixed
  DUPLICATE_ARGS_WARNING_COMMENT_DONE("duplicate_args_warning_comment_done", null, false, true, true),
  // Template for warning about duplicate arguments errors in a page
  DUPLICATE_ARGS_WARNING_TEMPLATE("duplicate_args_warning_template", null, false, true, false),
  // Comment for warning template about duplicate arguments errors in a page
  DUPLICATE_ARGS_WARNING_TEMPLATE_COMMENT("duplicate_args_warning_template_comment", null, false, true, false),
  // Help page
  HELP_PAGE("help_page", null, false, true, false),
  // URL of the help page
  HELP_URL("help_url", "http://en.wikipedia.org/wiki/Wikipedia:WPCleaner", false, true, false),
  // Comment used to indicate an ISBN with an error
  ISBN_HELP_NEEDED_COMMENT("general_isbn_help_needed_comment", null, false, true, true),
  // Page for storing a synthesis of ISBN errors
  ISBN_ERRORS_PAGE("isbn_errors_page", null, false, true, true),
  // Comment for storing a synthesis of ISBN errors
  ISBN_ERRORS_PAGE_COMMENT("isbn_errors_page_comment", "ISBN", false, true, true),
  // Comment for warning about ISBN errors in a page
  ISBN_WARNING_COMMENT("isbn_warning_comment", null, false, true, true),
  // Comment for warning about one ISBN error in a page
  ISBN_WARNING_COMMENT_1("isbn_warning_comment_1", null, false, true, true),
  // Comment for telling that ISBN errors have been fixed
  ISBN_WARNING_COMMENT_DONE("isbn_warning_comment_done", null, false, true, true),
  // Template for warning about ISBN errors in a page
  ISBN_WARNING_TEMPLATE("isbn_warning_template", null, false, true, false),
  // Comment for warning template about ISBN errors in a page
  ISBN_WARNING_TEMPLATE_COMMENT("isbn_warning_template_comment", null, false, true, false),
  // Comment used to indicate an ISSN with an error
  ISSN_HELP_NEEDED_COMMENT("general_issn_help_needed_comment", null, false, true, true),
  // Page for storing a synthesis of ISSN errors
  ISSN_ERRORS_PAGE("issn_errors_page", null, false, true, true),
  // Comment for storing a synthesis of ISSN errors
  ISSN_ERRORS_PAGE_COMMENT("issn_errors_page_comment", "ISSN", false, true, true),
  // Comment for warning about ISSN errors in a page
  ISSN_WARNING_COMMENT("issn_warning_comment", null, false, true, true),
  // Comment for warning about one ISSN error in a page
  ISSN_WARNING_COMMENT_1("issn_warning_comment_1", null, false, true, true),
  // Comment for telling that ISSN errors have been fixed
  ISSN_WARNING_COMMENT_DONE("issn_warning_comment_done", null, false, true, true),
  // Template for warning about ISSN errors in a page
  ISSN_WARNING_TEMPLATE("issn_warning_template", null, false, true, false),
  // Comment for warning template about ISSN errors in a page
  ISSN_WARNING_TEMPLATE_COMMENT("issn_warning_template_comment", null, false, true, false),
  // Template used to mark a text as being in a foreign language
  LANG_TEMPLATE("lang_template", null, false, true, false),
  // Template used to group all messages on a user talk page
  MSG_GLOBAL_LIST_TEMPLATE("rc_msg_global_list_template", null, false, true, true),
  // Template used to group all messages on a user talk page
  MSG_GLOBAL_TEMPLATE("rc_msg_global_template", null, false, true, true),
  // Title used to group all messages on a user talk page
  MSG_GLOBAL_TITLE("rc_msg_global_title", null, false, true, true),
  // Template used to warn a user who has created a new article with disambiguation links
  MSG_NEW_ARTICLE_MODIFIED_WITH_DAB_TEMPLATE("rc_msg_new_article_modified_with_dab_template", null, false, true, true),
  // Title used to warn a user who has created a new article with disambiguation links
  MSG_NEW_ARTICLE_MODIFIED_WITH_DAB_TITLE("rc_msg_new_article_modified_with_dab_title", null, false, true, true),
  // Template used to warn a user who has modified a new article with disambiguation links
  MSG_NEW_ARTICLE_MODIFIER_WITH_DAB_TEMPLATE("rc_msg_new_article_modifier_with_dab_template", null, false, true, true),
  // Title used to warn a user who has modified a new article with disambiguation links
  MSG_NEW_ARTICLE_MODIFIER_WITH_DAB_TITLE("rc_msg_new_article_modifier_with_dab_title", null, false, true, true),
  // Template used to warn a user who has created a new article with disambiguation links
  MSG_NEW_ARTICLE_WITH_DAB_TEMPLATE("rc_msg_new_article_with_dab_template", null, false, true, true),
  // Title used to warn a user who has created a new article with disambiguation links
  MSG_NEW_ARTICLE_WITH_DAB_TITLE("rc_msg_new_article_with_dab_title", null, false, true, true),
  // Template creating a "|"
  PIPE_TEMPLATE("general_pipe_template", "!", false, true, false),
  // Comment when adding categories to a redirect page
  REDIRECT_CATEGORIES_COMMENT("redirect_categories_comment", null, false, true, true),
  // Comment when adding templates to a redirect page
  REDIRECT_TEMPLATES_COMMENT("redirect_templates_comment", null, false, true, true),
  // Warning when replacing links to redirect pages by direct links
  REDIRECT_WARNING_BEFORE_REPLACEMENT("redirect_warning_before_replacement", null, true, true, true),
  // Preferred separator between consecutive <ref/> tags
  REF_SEPARATOR("general_ref_separator", null, true, true, true),
  // WPCleaner tag for modifications
  TAG("general_wpcleaner_tag", null, false, true, true),
  // "To do" sub-page
  TODO_SUBPAGE("general_todo_subpage", null, false, true, false),
  // Text to use in the "To do" sub-page for requesting its deletion
  TODO_SUBPAGE_DELETE("general_todo_subpage_delete", null, false, true, true),
  // Comment used when translating
  TRANSLATION_COMMENT("translation_comment", null, false, true, true),
  // User
  USER("general_user", null, false, false, true),
  // Wiktionary interwiki
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
