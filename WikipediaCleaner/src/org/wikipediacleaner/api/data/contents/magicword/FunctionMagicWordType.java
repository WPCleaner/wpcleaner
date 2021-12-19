/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.magicword;

import javax.annotation.Nonnull;

/**
 * Definition of an image magic word type.
 */
public class FunctionMagicWordType extends MagicWordType {

  // Names
  private static final String ID_ANCHOR_ENCODE = "anchorencode";
  private static final String ID_BASE_PAGE_NAME = "basepagename";
  private static final String ID_BASE_PAGE_NAME_E = "basepagnenamee";
  private static final String ID_CANONICAL_URL = "canonicalurl";
  private static final String ID_CONTENT_LANGUAGE = "contentlanguage";
  private static final String ID_CURRENT_DAY = "currentday";
  private static final String ID_CURRENT_DAY_2 = "currentday2";
  private static final String ID_CURRENT_DAY_NAME = "currentdayname";
  private static final String ID_CURRENT_DOW = "currentdow";
  private static final String ID_CURRENT_HOUR = "currenthour";
  private static final String ID_CURRENT_MONTH = "currentmonth";
  private static final String ID_CURRENT_MONTH_ABBREV = "currentmonthabbrev";
  private static final String ID_CURRENT_MONTH_NAME = "currentmonthname";
  private static final String ID_CURRENT_MONTH_NAME_GEN = "currentmonthnamegen";
  private static final String ID_CURRENT_TIME = "currenttime";
  private static final String ID_CURRENT_TIMESTAMP = "currenttimestamp";
  private static final String ID_CURRENT_VERSION = "currentversion";
  private static final String ID_CURRENT_WEEK = "currentweek";
  private static final String ID_CURRENT_YEAR = "currentyear";
  private static final String ID_DEFAULT_SORT = "defaultsort";
  private static final String ID_DIRECTION_MARK = "directionmark";
  private static final String ID_DISPLAY_TITLE = "displaytitle";
  private static final String ID_EXPLODE = "explode";
  private static final String ID_EXPR = "expr";
  private static final String ID_FILE_PATH = "filepath";
  private static final String ID_FORMAT_DATE = "formatdate";
  private static final String ID_FORMAT_NUM = "formatnum";
  private static final String ID_FULL_PAGE_NAME = "fullpagename";
  private static final String ID_FULL_PAGE_NAME_E = "fullpagenamee";
  private static final String ID_FULL_URL = "fullurl";
  private static final String ID_GENDER = "gender";
  private static final String ID_GRAMMAR = "grammar";
  private static final String ID_IF = "if";
  private static final String ID_IF_EQ = "ifeq";
  private static final String ID_IF_ERROR = "iferror";
  private static final String ID_IF_EXIST = "ifexist";
  private static final String ID_IF_EXPR = "ifexpr";
  private static final String ID_INVOKE = "invoke";
  private static final String ID_LANGUAGE = "language";
  private static final String ID_LC = "lc";
  private static final String ID_LC_FIRST = "lcfirst";
  private static final String ID_LEN = "len";
  private static final String ID_LOCAL_DAY = "localday";
  private static final String ID_LOCAL_DAY_2 = "localday2";
  private static final String ID_LOCAL_DAY_NAME = "localdayname";
  private static final String ID_LOCAL_DOW = "localdow";
  private static final String ID_LOCAL_HOUR = "localhour";
  private static final String ID_LOCAL_MONTH = "localmonth";
  private static final String ID_LOCAL_MONTH_ABBREV = "localmonthabbrev";
  private static final String ID_LOCAL_MONTH_NAME = "localmonthname";
  private static final String ID_LOCAL_MONTH_NAME_GEN = "localmonthnamegen";
  private static final String ID_LOCAL_TIME = "localtime";
  private static final String ID_LOCAL_TIMESTAMP = "localtimestamp";
  private static final String ID_LOCAL_URL = "localurl";
  private static final String ID_LOCAL_WEEK = "localweek";
  private static final String ID_LOCAL_YEAR = "localyear";
  private static final String ID_LST = "lst"; // Extension:Labeled Section Transclusion
  private static final String ID_LST_H = "lsth"; // Extension:Labeled Section Transclusion
  private static final String ID_LST_X = "lstx"; // Extension:Labeled Section Transclusion
  private static final String ID_NAMESPACE = "namespace";
  private static final String ID_NAMESPACE_E = "namespacee";
  private static final String ID_NAMESPACE_NUMBER = "namespacenumber";
  private static final String ID_NS = "ns";
  private static final String ID_NUMBER_IN_GROUP = "numberingroup";
  private static final String ID_NUMBER_OF_ACTIVE_USERS = "numberofactiveusers";
  private static final String ID_NUMBER_OF_ADMINS = "numberofadmins";
  private static final String ID_NUMBER_OF_ARTICLES = "numberofarticles";
  private static final String ID_NUMBER_OF_EDITS = "numberofedits";
  private static final String ID_NUMBER_OF_FILES = "numberoffiles";
  private static final String ID_NUMBER_OF_PAGES = "numberofpages";
  private static final String ID_NUMBER_OF_USERS = "numberofusers";
  private static final String ID_NUMBER_OF_VIEWS = "numberofviews";
  private static final String ID_PAD_LEFT = "padleft";
  private static final String ID_PAD_RIGHT = "padright";
  private static final String ID_PAGE_ID = "pageid";
  private static final String ID_PAGE_NAME = "pagename";
  private static final String ID_PAGE_NAME_E = "pagenamee";
  private static final String ID_PAGE_SIZE = "pagesize";
  private static final String ID_PAGES_IN_CATEGORY = "pagesincategory";
  private static final String ID_PAGES_IN_NAMESPACE = "pagesinnamespace";
  private static final String ID_PLURAL = "plural";
  private static final String ID_POS = "pos";
  private static final String ID_PROTECTION_LEVEL = "protectionlevel";
  private static final String ID_REL_2_ABS = "rel2abs";
  private static final String ID_REPLACE = "replace";
  private static final String ID_REVISION_DAY = "revisionday";
  private static final String ID_REVISION_DAY_2 = "revisionday2";
  private static final String ID_REVISION_ID = "revisionid";
  private static final String ID_REVISION_MONTH = "revisionmonth";
  private static final String ID_REVISION_MONTH_1 = "revisionmonth1";
  private static final String ID_REVISION_TIMESTAMP = "revisiontimestamp";
  private static final String ID_REVISION_USER = "revisionuser";
  private static final String ID_REVISION_YEAR = "revisionyear";
  private static final String ID_RPOS = "rpos";
  private static final String ID_SAFE_SUBST = "safesubst";
  private static final String ID_SCRIPT_PATH = "scriptpath";
  private static final String ID_SECTION = "section"; // Extension:Labeled Section Transclusion
  private static final String ID_SECTION_H = "section-h"; // Extension:Labeled Section Transclusion
  private static final String ID_SECTION_X = "section-x"; // Extension:Labeled Section Transclusion
  private static final String ID_SERVER = "server";
  private static final String ID_SERVER_NAME = "servername";
  private static final String ID_SITE_NAME = "sitename";
  private static final String ID_SPECIAL = "special";
  private static final String ID_SPECIAL_E = "speciale";
  private static final String ID_STYLE_PATH = "stylepath";
  private static final String ID_SUB = "sub";
  private static final String ID_SUBPAGE_NAME = "subpagename";
  private static final String ID_SUBPAGE_NAME_E = "subpagenamee";
  private static final String ID_SUBJECT_PAGE_NAME = "subjectpagename";
  private static final String ID_SUBJECT_PAGE_NAME_E = "subjectpagenamee";
  private static final String ID_SUBJECT_SPACE = "subjectspace";
  private static final String ID_SUBJECT_SPACE_E = "subjectspacee";
  private static final String ID_SUBST = "subst";
  private static final String ID_SWITCH = "switch";
  private static final String ID_TAG = "tag";
  private static final String ID_TALK_PAGE_NAME = "talkpagename";
  private static final String ID_TALK_PAGE_NAME_E = "talkpagenamee";
  private static final String ID_TALK_SPACE = "talkspace";
  private static final String ID_TALK_SPACE_E = "talkspacee";
  private static final String ID_TIME = "time";
  private static final String ID_TIME_L = "timel";
  private static final String ID_TITLE_PARTS = "titleparts";
  private static final String ID_UC = "uc";
  private static final String ID_UC_FIRST = "ucfirst";
  private static final String ID_URL_DECODE = "urldecode";
  private static final String ID_URL_ENCODE = "urlencode";

  // Magic words
  public static final FunctionMagicWordType ANCHOR_ENCODE = createFunctionType(ID_ANCHOR_ENCODE);
  public static final FunctionMagicWordType BASE_PAGE_NAME = createFunctionType(ID_BASE_PAGE_NAME);
  public static final FunctionMagicWordType BASE_PAGE_NAME_E = createFunctionType(ID_BASE_PAGE_NAME_E);
  public static final FunctionMagicWordType CANONICAL_URL = createFunctionType(ID_CANONICAL_URL);
  public static final FunctionMagicWordType CONTENT_LANGUAGE = createFunctionType(ID_CONTENT_LANGUAGE);
  public static final FunctionMagicWordType CURRENT_DAY = createFunctionType(ID_CURRENT_DAY);
  public static final FunctionMagicWordType CURRENT_DAY_2 = createFunctionType(ID_CURRENT_DAY_2);
  public static final FunctionMagicWordType CURRENT_DAY_NAME = createFunctionType(ID_CURRENT_DAY_NAME);
  public static final FunctionMagicWordType CURRENT_DOW = createFunctionType(ID_CURRENT_DOW);
  public static final FunctionMagicWordType CURRENT_HOUR = createFunctionType(ID_CURRENT_HOUR);
  public static final FunctionMagicWordType CURRENT_MONTH = createFunctionType(ID_CURRENT_MONTH);
  public static final FunctionMagicWordType CURRENT_MONTH_ABBREV = createFunctionType(ID_CURRENT_MONTH_ABBREV);
  public static final FunctionMagicWordType CURRENT_MONTH_NAME = createFunctionType(ID_CURRENT_MONTH_NAME);
  public static final FunctionMagicWordType CURRENT_MONTH_NAME_GEN = createFunctionType(ID_CURRENT_MONTH_NAME_GEN);
  public static final FunctionMagicWordType CURRENT_TIME = createFunctionType(ID_CURRENT_TIME);
  public static final FunctionMagicWordType CURRENT_TIMESTAMP = createFunctionType(ID_CURRENT_TIMESTAMP);
  public static final FunctionMagicWordType CURRENT_VERSION = createFunctionType(ID_CURRENT_VERSION);
  public static final FunctionMagicWordType CURRENT_WEEK = createFunctionType(ID_CURRENT_WEEK);
  public static final FunctionMagicWordType CURRENT_YEAR = createFunctionType(ID_CURRENT_YEAR);
  public static final FunctionMagicWordType DEFAULT_SORT = createFunctionType(ID_DEFAULT_SORT);
  public static final FunctionMagicWordType DIRECTION_MARK = createFunctionType(ID_DIRECTION_MARK);
  public static final FunctionMagicWordType DISPLAY_TITLE = createFunctionType(ID_DISPLAY_TITLE);
  public static final FunctionMagicWordType EXPLODE = createFunctionTypeWithSharp(ID_EXPLODE);
  public static final FunctionMagicWordType EXPR = createFunctionTypeWithSharp(ID_EXPR);
  public static final FunctionMagicWordType FILE_PATH = createFunctionType(ID_FILE_PATH);
  public static final FunctionMagicWordType FORMAT_DATE = createFunctionTypeWithSharp(ID_FORMAT_DATE);
  public static final FunctionMagicWordType FORMAT_NUM = createFunctionTypeWithoutPST(ID_FORMAT_NUM);
  public static final FunctionMagicWordType FULL_PAGE_NAME = createFunctionType(ID_FULL_PAGE_NAME);
  public static final FunctionMagicWordType FULL_PAGE_NAME_E = createFunctionType(ID_FULL_PAGE_NAME_E);
  public static final FunctionMagicWordType FULL_URL = createFunctionType(ID_FULL_URL);
  public static final FunctionMagicWordType GENDER = createFunctionType(ID_GENDER);
  public static final FunctionMagicWordType GRAMMAR = createFunctionType(ID_GRAMMAR);
  public static final FunctionMagicWordType IF = createFunctionTypeWithSharp(ID_IF);
  public static final FunctionMagicWordType IF_EQ = createFunctionTypeWithSharp(ID_IF_EQ);
  public static final FunctionMagicWordType IF_ERROR = createFunctionTypeWithSharp(ID_IF_ERROR);
  public static final FunctionMagicWordType IF_EXIST = createFunctionTypeWithSharp(ID_IF_EXIST);
  public static final FunctionMagicWordType IF_EXPR = createFunctionTypeWithSharp(ID_IF_EXPR);
  public static final FunctionMagicWordType INVOKE = createFunctionTypeWithSharp(ID_INVOKE);
  public static final FunctionMagicWordType LANGUAGE = createFunctionTypeWithSharp(ID_LANGUAGE);
  public static final FunctionMagicWordType LC = createFunctionType(ID_LC);
  public static final FunctionMagicWordType LC_FIRST = createFunctionType(ID_LC_FIRST);
  public static final FunctionMagicWordType LEN = createFunctionTypeWithSharp(ID_LEN);
  public static final FunctionMagicWordType LOCAL_DAY = createFunctionType(ID_LOCAL_DAY);
  public static final FunctionMagicWordType LOCAL_DAY_2 = createFunctionType(ID_LOCAL_DAY_2);
  public static final FunctionMagicWordType LOCAL_DAY_NAME = createFunctionType(ID_LOCAL_DAY_NAME);
  public static final FunctionMagicWordType LOCAL_DOW = createFunctionType(ID_LOCAL_DOW);
  public static final FunctionMagicWordType LOCAL_HOUR = createFunctionType(ID_LOCAL_HOUR);
  public static final FunctionMagicWordType LOCAL_MONTH = createFunctionType(ID_LOCAL_MONTH);
  public static final FunctionMagicWordType LOCAL_MONTH_ABBREV = createFunctionType(ID_LOCAL_MONTH_ABBREV);
  public static final FunctionMagicWordType LOCAL_MONTH_NAME = createFunctionType(ID_LOCAL_MONTH_NAME);
  public static final FunctionMagicWordType LOCAL_MONTH_NAME_GEN = createFunctionType(ID_LOCAL_MONTH_NAME_GEN);
  public static final FunctionMagicWordType LOCAL_TIME = createFunctionType(ID_LOCAL_TIME);
  public static final FunctionMagicWordType LOCAL_TIMESTAMP = createFunctionType(ID_LOCAL_TIMESTAMP);
  public static final FunctionMagicWordType LOCAL_URL = createFunctionType(ID_LOCAL_URL);
  public static final FunctionMagicWordType LOCAL_WEEK = createFunctionType(ID_LOCAL_WEEK);
  public static final FunctionMagicWordType LOCAL_YEAR = createFunctionType(ID_LOCAL_YEAR);
  public static final FunctionMagicWordType LST = createFunctionTypeWithSharp(ID_LST);
  public static final FunctionMagicWordType LST_H = createFunctionTypeWithSharp(ID_LST_H);
  public static final FunctionMagicWordType LST_X = createFunctionTypeWithSharp(ID_LST_X);
  public static final FunctionMagicWordType NAMESPACE = createFunctionType(ID_NAMESPACE);
  public static final FunctionMagicWordType NAMESPACE_E = createFunctionType(ID_NAMESPACE_E);
  public static final FunctionMagicWordType NAMESPACE_NUMBER = createFunctionType(ID_NAMESPACE_NUMBER);
  public static final FunctionMagicWordType NS = createFunctionType(ID_NS);
  public static final FunctionMagicWordType NUMBER_IN_GROUP = createFunctionType(ID_NUMBER_IN_GROUP);
  public static final FunctionMagicWordType NUMBER_OF_ACTIVE_USERS = createFunctionType(ID_NUMBER_OF_ACTIVE_USERS);
  public static final FunctionMagicWordType NUMBER_OF_ADMINS = createFunctionType(ID_NUMBER_OF_ADMINS);
  public static final FunctionMagicWordType NUMBER_OF_ARTICLES = createFunctionType(ID_NUMBER_OF_ARTICLES);
  public static final FunctionMagicWordType NUMBER_OF_EDITS = createFunctionType(ID_NUMBER_OF_EDITS);
  public static final FunctionMagicWordType NUMBER_OF_FILES = createFunctionType(ID_NUMBER_OF_FILES);
  public static final FunctionMagicWordType NUMBER_OF_PAGES = createFunctionType(ID_NUMBER_OF_PAGES);
  public static final FunctionMagicWordType NUMBER_OF_USERS = createFunctionType(ID_NUMBER_OF_USERS);
  public static final FunctionMagicWordType NUMBER_OF_VIEWS = createFunctionType(ID_NUMBER_OF_VIEWS);
  public static final FunctionMagicWordType PAD_LEFT = createFunctionType(ID_PAD_LEFT);
  public static final FunctionMagicWordType PAD_RIGHT = createFunctionType(ID_PAD_RIGHT);
  public static final FunctionMagicWordType PAGE_ID = createFunctionType(ID_PAGE_ID);
  public static final FunctionMagicWordType PAGE_NAME = createFunctionType(ID_PAGE_NAME);
  public static final FunctionMagicWordType PAGE_NAME_E = createFunctionType(ID_PAGE_NAME_E);
  public static final FunctionMagicWordType PAGE_SIZE = createFunctionType(ID_PAGE_SIZE);
  public static final FunctionMagicWordType PAGES_IN_CATEGORY = createFunctionType(ID_PAGES_IN_CATEGORY);
  public static final FunctionMagicWordType PAGES_IN_NAMESPACE = createFunctionType(ID_PAGES_IN_NAMESPACE);
  public static final FunctionMagicWordType PLURAL = createFunctionType(ID_PLURAL);
  public static final FunctionMagicWordType POS = createFunctionTypeWithSharp(ID_POS);
  public static final FunctionMagicWordType PROTECTION_LEVEL = createFunctionType(ID_PROTECTION_LEVEL);
  public static final FunctionMagicWordType REL_2_ABS = createFunctionTypeWithSharp(ID_REL_2_ABS);
  public static final FunctionMagicWordType REPLACE = createFunctionTypeWithSharp(ID_REPLACE);
  public static final FunctionMagicWordType REVISION_DAY = createFunctionType(ID_REVISION_DAY);
  public static final FunctionMagicWordType REVISION_DAY_2 = createFunctionType(ID_REVISION_DAY_2);
  public static final FunctionMagicWordType REVISION_ID = createFunctionType(ID_REVISION_ID);
  public static final FunctionMagicWordType REVISION_MONTH = createFunctionType(ID_REVISION_MONTH);
  public static final FunctionMagicWordType REVISION_MONTH_1 = createFunctionType(ID_REVISION_MONTH_1);
  public static final FunctionMagicWordType REVISION_TIMESTAMP = createFunctionType(ID_REVISION_TIMESTAMP);
  public static final FunctionMagicWordType REVISION_USER = createFunctionType(ID_REVISION_USER);
  public static final FunctionMagicWordType REVISION_YEAR = createFunctionType(ID_REVISION_YEAR);
  public static final FunctionMagicWordType RPOS = createFunctionTypeWithSharp(ID_RPOS);
  public static final FunctionMagicWordType SAFE_SUBST = createFunctionType(ID_SAFE_SUBST);
  public static final FunctionMagicWordType SCRIPT_PATH = createFunctionType(ID_SCRIPT_PATH);
  public static final FunctionMagicWordType SECTION = createFunctionTypeWithSharp(ID_SECTION);
  public static final FunctionMagicWordType SECTION_H = createFunctionTypeWithSharp(ID_SECTION_H);
  public static final FunctionMagicWordType SECTION_X = createFunctionTypeWithSharp(ID_SECTION_X);
  public static final FunctionMagicWordType SERVER = createFunctionType(ID_SERVER);
  public static final FunctionMagicWordType SERVER_NAME = createFunctionType(ID_SERVER_NAME);
  public static final FunctionMagicWordType SITE_NAME = createFunctionType(ID_SITE_NAME);
  public static final FunctionMagicWordType SPECIAL = createFunctionTypeWithSharp(ID_SPECIAL);
  public static final FunctionMagicWordType SPECIAL_E = createFunctionTypeWithSharp(ID_SPECIAL_E);
  public static final FunctionMagicWordType STYLE_PATH = createFunctionType(ID_STYLE_PATH);
  public static final FunctionMagicWordType SUB = createFunctionTypeWithSharp(ID_SUB);
  public static final FunctionMagicWordType SUBPAGE_NAME = createFunctionType(ID_SUBPAGE_NAME);
  public static final FunctionMagicWordType SUBPAGE_NAME_E = createFunctionType(ID_SUBPAGE_NAME_E);
  public static final FunctionMagicWordType SUBJECT_PAGE_NAME = createFunctionType(ID_SUBJECT_PAGE_NAME);
  public static final FunctionMagicWordType SUBJECT_PAGE_NAME_E = createFunctionType(ID_SUBJECT_PAGE_NAME_E);
  public static final FunctionMagicWordType SUBJECT_SPACE = createFunctionType(ID_SUBJECT_SPACE);
  public static final FunctionMagicWordType SUBJECT_SPACE_E = createFunctionType(ID_SUBJECT_SPACE_E);
  public static final FunctionMagicWordType SUBST = createFunctionType(ID_SUBST);
  public static final FunctionMagicWordType SWITCH = createFunctionTypeWithSharp(ID_SWITCH);
  public static final FunctionMagicWordType TAG = createFunctionTypeWithSharp(ID_TAG);
  public static final FunctionMagicWordType TALK_PAGE_NAME = createFunctionType(ID_TALK_PAGE_NAME);
  public static final FunctionMagicWordType TALK_PAGE_NAME_E = createFunctionType(ID_TALK_PAGE_NAME_E);
  public static final FunctionMagicWordType TALK_SPACE = createFunctionType(ID_TALK_SPACE);
  public static final FunctionMagicWordType TALK_SPACE_E = createFunctionType(ID_TALK_SPACE_E);
  public static final FunctionMagicWordType TIME = createFunctionTypeWithSharp(ID_TIME);
  public static final FunctionMagicWordType TIME_L = createFunctionType(ID_TIME_L);
  public static final FunctionMagicWordType TITLE_PARTS = createFunctionTypeWithSharp(ID_TITLE_PARTS);
  public static final FunctionMagicWordType UC = createFunctionType(ID_UC);
  public static final FunctionMagicWordType UC_FIRST = createFunctionType(ID_UC_FIRST);
  public static final FunctionMagicWordType URL_DECODE = createFunctionType(ID_URL_DECODE);
  public static final FunctionMagicWordType URL_ENCODE = createFunctionType(ID_URL_ENCODE);

  /**
   * Register magic word types.
   */
  static void registerMagicWordTypes() {
    // Do nothing, magic words register by themselves
  }

  /**
   * Create a function magic word type.
   * 
   * @param name Name of the magic word type.
   * @return Magic word type.
   */
  private static FunctionMagicWordType createFunctionType(@Nonnull String name) {
    return new FunctionMagicWordType(name, false, false);
  }

  /**
   * Create a function magic word type which requires a # to be used.
   * 
   * @param name Name of the magic word type.
   * @return Magic word type.
   */
  private static FunctionMagicWordType createFunctionTypeWithSharp(@Nonnull String name) {
    return new FunctionMagicWordType(name, true, false);
  }

  /**
   * Create a function magic word type which changes nothing when parsing in PST mode..
   * 
   * @param name Name of the magic word type.
   * @return Magic word type.
   */
  private static FunctionMagicWordType createFunctionTypeWithoutPST(@Nonnull String name) {
    return new FunctionMagicWordType(name, false, true);
  }

  /**
   * Create a function magic word type.
   * 
   * @param name Name of the magic word type.
   * @param withSharp True if the magic word needs a # to be used.
   * @param withoutPST True if the magic word changes nothing when parsing in PST mode (PreSave Transform).
   */
  private FunctionMagicWordType(
      @Nonnull String name,
      boolean withSharp,
      boolean withoutPST) {
    super(name, false, true, withSharp, withoutPST);
  }

}
