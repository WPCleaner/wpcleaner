/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.magicword;

import javax.annotation.Nonnull;

/**
 * Definition of a simple magic word type.
 */
public class SimpleMagicWordType extends MagicWordType {

  // Names
  private static final String ID_ABBREVIATE = "abbreviate";
  private static final String ID_ARTICLE_PATH = "articlepath";
  private static final String ID_BABEL = "babel";
  private static final String ID_CANONICAL_URL_E = "canonicalurle";
  private static final String ID_CATEGORY_TREE = "categorytree";
  private static final String ID_CONTRIBUTION_TOTAL = "contributiontotal";
  private static final String ID_COUNT = "count";
  private static final String ID_COORDINATES = "coordinates";
  private static final String ID_CURRENT_MONTH_1 = "currentmonth1";
  private static final String ID_DECIMAL_PLACES = "decimalplaces";
  private static final String ID_DEFAULT = "default";
  private static final String ID_DEFAULT_SORT_NOERROR = "defaultsort_noerror";
  private static final String ID_DEFAULT_SORT_NOREPLACE = "defaultsort_noreplace";
  private static final String ID_FORCE_TOC = "forcetoc";
  private static final String ID_FULL_URL_E = "fullurle";
  private static final String ID_HIDDEN_CAT = "hiddencat";
  private static final String ID_INDEX = "index";
  private static final String ID_INFO_PAGE = "infopage";
  private static final String ID_INT = "int";
  private static final String ID_LINK_UNIT = "linkunit";
  private static final String ID_LOCAL_MONTH_1 = "localmonth1";
  private static final String ID_LOCAL_URL_E = "localurle";
  private static final String ID_LQT_PAGE_LIMIT = "lqtpagelimit";
  private static final String ID_MSG = "msg";
  private static final String ID_MSGNW = "msgnw";
  private static final String ID_NEW_SECTION_LINK = "newsectionlink";
  private static final String ID_NO_CONTENT_CONVERT = "nocontentconvert";
  private static final String ID_NO_EDIT_SECTION = "noeditsection";
  private static final String ID_NO_GALLERY = "nogallery";
  private static final String ID_NO_HEADER = "noheader";
  private static final String ID_NO_INDEX = "noindex";
  private static final String ID_NO_NEW_SECTION_LINK = "nonewsectionlink";
  private static final String ID_NO_TITLE_CONVERT = "notitleconvert";
  private static final String ID_NO_TOC = "notoc";
  private static final String ID_NSE = "nse";
  private static final String ID_OGG_NOICON = "ogg_noicon";
  private static final String ID_OGG_NOPLAYER = "ogg_noplayer";
  private static final String ID_OGG_THUMBTIME = "ogg_thumbtime";
  private static final String ID_PAGES_IN_CATEGORY_ALL = "pagesincategory_all";
  private static final String ID_PAGES_IN_CATEGORY_FILES = "pagesincategory_files";
  private static final String ID_PAGES_IN_CATEGORY_PAGES = "pagesincategory_pages";
  private static final String ID_PAGES_IN_CATEGORY_SUBCATS = "pagesincategory_subcats";
  private static final String ID_PAGES_USING_PENDING_CHANGES = "pagesusingpendingchanges";
  private static final String ID_PENDING_CHANGE_LEVEL = "pendingchangelevel";
  private static final String ID_RAW = "raw";
  private static final String ID_RAW_SUFFIX = "rawsuffix";
  private static final String ID_REDIRECT = "redirect";
  private static final String ID_SIGNIFICANT_FIGURES = "significantfigures";
  private static final String ID_SOURCE_UNIT = "sourceunit";
  private static final String ID_STATIC_REDIRECT = "staticredirect";
  private static final String ID_SWITCH_COUNTRY = "switchcountry";
  private static final String ID_SWITCH_LANGUAGE = "switchlanguage";
  private static final String ID_TARGET_UNIT = "targetunit";
  private static final String ID_TOC = "toc";
  private static final String ID_TRANSLATION_DIALOG = "translationdialog";
  private static final String ID_URL_PATH = "url_path";
  private static final String ID_URL_QUERY = "url_query";
  private static final String ID_URL_WIKI = "url_wiki";
  private static final String ID_USE_LIQUID_THREADS = "useliquidthreads";
  private static final String ID_USER_TEST_WIKI = "usertestwiki";

  // Magic words
  public static final SimpleMagicWordType ABBREVIATE = createSimpleType(ID_ABBREVIATE);
  public static final SimpleMagicWordType ARTICLE_PATH = createSimpleType(ID_ARTICLE_PATH);
  public static final SimpleMagicWordType BABEL = createSimpleType(ID_BABEL);
  public static final SimpleMagicWordType CANONICAL_URL_E = createSimpleType(ID_CANONICAL_URL_E);
  public static final SimpleMagicWordType CATEGORY_TREE = createSimpleType(ID_CATEGORY_TREE);
  public static final SimpleMagicWordType CONTRIBUTION_TOTAL = createSimpleType(ID_CONTRIBUTION_TOTAL);
  public static final SimpleMagicWordType COUNT = createSimpleType(ID_COUNT);
  public static final SimpleMagicWordType COORDINATES = createSimpleType(ID_COORDINATES);
  public static final SimpleMagicWordType CURRENT_MONTH_1 = createSimpleType(ID_CURRENT_MONTH_1);
  public static final SimpleMagicWordType DECIMAL_PLACES = createSimpleType(ID_DECIMAL_PLACES);
  public static final SimpleMagicWordType DEFAULT = createSimpleType(ID_DEFAULT);
  public static final SimpleMagicWordType DEFAULT_SORT_NOERROR = createSimpleType(ID_DEFAULT_SORT_NOERROR);
  public static final SimpleMagicWordType DEFAULT_SORT_NOREPLACE = createSimpleType(ID_DEFAULT_SORT_NOREPLACE);
  public static final SimpleMagicWordType FORCE_TOC = createSimpleType(ID_FORCE_TOC);
  public static final SimpleMagicWordType FULL_URL_E = createSimpleType(ID_FULL_URL_E);
  public static final SimpleMagicWordType HIDDEN_CAT = createSimpleType(ID_HIDDEN_CAT);
  public static final SimpleMagicWordType INDEX = createSimpleType(ID_INDEX);
  public static final SimpleMagicWordType INFO_PAGE = createSimpleType(ID_INFO_PAGE);
  public static final SimpleMagicWordType INT = createSimpleType(ID_INT);
  public static final SimpleMagicWordType LINK_UNIT = createSimpleType(ID_LINK_UNIT);
  public static final SimpleMagicWordType LOCAL_MONTH_1 = createSimpleType(ID_LOCAL_MONTH_1);
  public static final SimpleMagicWordType LOCAL_URL_E = createSimpleType(ID_LOCAL_URL_E);
  public static final SimpleMagicWordType LQT_PAGE_LIMIT = createSimpleType(ID_LQT_PAGE_LIMIT);
  public static final SimpleMagicWordType MSG = createSimpleType(ID_MSG);
  public static final SimpleMagicWordType MSGNW = createSimpleType(ID_MSGNW);
  public static final SimpleMagicWordType NEW_SECTION_LINK = createSimpleType(ID_NEW_SECTION_LINK);
  public static final SimpleMagicWordType NO_CONTENT_CONVERT = createSimpleType(ID_NO_CONTENT_CONVERT);
  public static final SimpleMagicWordType NO_EDIT_SECTION = createSimpleType(ID_NO_EDIT_SECTION);
  public static final SimpleMagicWordType NO_GALLERY = createSimpleType(ID_NO_GALLERY);
  public static final SimpleMagicWordType NO_HEADER = createSimpleType(ID_NO_HEADER);
  public static final SimpleMagicWordType NO_INDEX = createSimpleType(ID_NO_INDEX);
  public static final SimpleMagicWordType NO_NEW_SECTION_LINK = createSimpleType(ID_NO_NEW_SECTION_LINK);
  public static final SimpleMagicWordType NO_TITLE_CONVERT = createSimpleType(ID_NO_TITLE_CONVERT);
  public static final SimpleMagicWordType NO_TOC = createSimpleType(ID_NO_TOC);
  public static final SimpleMagicWordType NSE = createSimpleType(ID_NSE);
  public static final SimpleMagicWordType OGG_NOICON = createSimpleType(ID_OGG_NOICON);
  public static final SimpleMagicWordType OGG_NOPLAYER = createSimpleType(ID_OGG_NOPLAYER);
  public static final SimpleMagicWordType OGG_THUMBTIME = createSimpleType(ID_OGG_THUMBTIME);
  public static final SimpleMagicWordType PAGES_IN_CATEGORY_ALL = createSimpleType(ID_PAGES_IN_CATEGORY_ALL);
  public static final SimpleMagicWordType PAGES_IN_CATEGORY_FILES = createSimpleType(ID_PAGES_IN_CATEGORY_FILES);
  public static final SimpleMagicWordType PAGES_IN_CATEGORY_PAGES = createSimpleType(ID_PAGES_IN_CATEGORY_PAGES);
  public static final SimpleMagicWordType PAGES_IN_CATEGORY_SUBCAT = createSimpleType(ID_PAGES_IN_CATEGORY_SUBCATS);
  public static final SimpleMagicWordType PAGES_USING_PENDING_CHANGES = createSimpleType(ID_PAGES_USING_PENDING_CHANGES);
  public static final SimpleMagicWordType PENDING_CHANGE_LEVEL = createSimpleType(ID_PENDING_CHANGE_LEVEL);
  public static final SimpleMagicWordType RAW = createSimpleType(ID_RAW); 
  public static final SimpleMagicWordType RAW_SUFFIX = createSimpleType(ID_RAW_SUFFIX);
  public static final SimpleMagicWordType REDIRECT = createSimpleType(ID_REDIRECT);
  public static final SimpleMagicWordType SIGNIFICANT_FIGURES = createSimpleType(ID_SIGNIFICANT_FIGURES);
  public static final SimpleMagicWordType SOURCE_UNIT = createSimpleType(ID_SOURCE_UNIT);
  public static final SimpleMagicWordType STATIC_REDIRECT = createSimpleType(ID_STATIC_REDIRECT);
  public static final SimpleMagicWordType SWITCH_COUNTRY = createSimpleType(ID_SWITCH_COUNTRY);
  public static final SimpleMagicWordType SWITCH_LANGUAGE = createSimpleType(ID_SWITCH_LANGUAGE);
  public static final SimpleMagicWordType TARGET_UNIT = createSimpleType(ID_TARGET_UNIT);
  public static final SimpleMagicWordType TOC = createSimpleType(ID_TOC);
  public static final SimpleMagicWordType TRANSLATION_DIALOG = createSimpleType(ID_TRANSLATION_DIALOG);
  public static final SimpleMagicWordType URL_PATH = createSimpleType(ID_URL_PATH);
  public static final SimpleMagicWordType URL_QUERY = createSimpleType(ID_URL_QUERY);
  public static final SimpleMagicWordType URL_WIKI = createSimpleType(ID_URL_WIKI);
  public static final SimpleMagicWordType USE_LIQUID_THREADS = createSimpleType(ID_USE_LIQUID_THREADS);
  public static final SimpleMagicWordType USER_TEST_WIKI = createSimpleType(ID_USER_TEST_WIKI);

  /**
   * Register magic word types.
   */
  static void registerMagicWordTypes() {
    // Do nothing, magic words register by themselves
  }

  /**
   * Create a magic word type.
   * 
   * @param name Name of the magic word type.
   * @return Magic word type.
   */
  private static SimpleMagicWordType createSimpleType(@Nonnull String name) {
    return new SimpleMagicWordType(name);
  }

  /**
   * Create a magic word type.
   * 
   * @param name Name of the magic word type.
   */
  private SimpleMagicWordType(
      @Nonnull String name) {
    super(name, false, false, false, false);
  }

}
