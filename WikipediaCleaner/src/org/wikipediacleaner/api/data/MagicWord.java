/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;


/**
 * Information about magic words.
 */
public class MagicWord implements Comparable<MagicWord> {

  /**
   * Magic word name.
   */
  private final String name;

  /**
   * List of magic word aliases.
   */
  private final List<String> aliases;

  /**
   * Flag indicating if the name is case sensitive.
   */
  private boolean caseSensitive;

  public final static String ABBREVIATE                  = "abbreviate";
  public final static String ARTICLE_PATH                = "articlepath";
  public final static String ANCHOR_ENCODE               = "anchorencode";
  public final static String BABEL                       = "babel";
  public final static String BASE_PAGE_NAME              = "basepagename";
  public final static String BASE_PAGE_NAME_E            = "basepagnenamee";
  public final static String CANONICAL_URL               = "canonicalurl";
  public final static String CANONICAL_URL_E             = "canonicalurle";
  public final static String CATEGORY_TREE               = "categorytree";
  public final static String CONTENT_LANGUAGE            = "contentlanguage";
  public final static String CONTRIBUTION_TOTAL          = "contributiontotal";
  public final static String COUNT                       = "count";
  public final static String COORDINATES                 = "coordinates";
  public final static String CURRENT_DAY                 = "currentday";
  public final static String CURRENT_DAY_2               = "currentday2";
  public final static String CURRENT_DAY_NAME            = "currentdayname";
  public final static String CURRENT_DOW                 = "currentdow";
  public final static String CURRENT_HOUR                = "currenthour";
  public final static String CURRENT_MONTH               = "currentmonth";
  public final static String CURRENT_MONTH_1             = "currentmonth1";
  public final static String CURRENT_MONTH_ABBREV        = "currentmonthabbrev";
  public final static String CURRENT_MONTH_NAME          = "currentmonthname";
  public final static String CURRENT_MONTH_NAME_GEN      = "currentmonthnamegen";
  public final static String CURRENT_TIME                = "currenttime";
  public final static String CURRENT_TIMESTAMP           = "currenttimestamp";
  public final static String CURRENT_VERSION             = "currentversion";
  public final static String CURRENT_WEEK                = "currentweek";
  public final static String CURRENT_YEAR                = "currentyear";
  public final static String DECIMAL_PLACES              = "decimalplaces";
  public final static String DEFAULT                     = "default";
  public final static String DEFAULT_SORT                = "defaultsort";
  public final static String DEFAULT_SORT_NOERROR        = "defaultsort_noerror";
  public final static String DEFAULT_SORT_NOREPLACE      = "defaultsort_noreplace";
  public final static String DIRECTION_MARK              = "directionmark";
  public final static String DISPLAY_TITLE               = "displaytitle";
  public final static String EXPLODE                     = "explode";
  public final static String EXPR                        = "expr";
  public final static String FILE_PATH                   = "filepath";
  public final static String FORCE_TOC                   = "forcetoc";
  public final static String FORMAT_DATE                 = "formatdate";
  public final static String FORMAT_NUM                  = "formatnum";
  public final static String FULL_PAGE_NAME              = "fullpagename";
  public final static String FULL_PAGE_NAME_E            = "fullpagenamee";
  public final static String FULL_URL                    = "fullurl";
  public final static String FULL_URL_E                  = "fullurle";
  public final static String GENDER                      = "gender";
  public final static String GRAMMAR                     = "grammar";
  public final static String HIDDEN_CAT                  = "hiddencat";
  public final static String IF                          = "if";
  public final static String IF_EQ                       = "ifeq";
  public final static String IF_ERROR                    = "iferror";
  public final static String IF_EXIST                    = "ifexist";
  public final static String IF_EXPR                     = "ifexpr";
  public final static String IMG_ALT                     = "img_alt";
  public final static String IMG_BASELINE                = "img_baseline";
  public final static String IMG_BORDER                  = "img_border";
  public final static String IMG_BOTTOM                  = "img_bottom";
  public final static String IMG_CENTER                  = "img_center";
  public final static String IMG_CLASS                   = "img_class";
  public final static String IMG_FRAMED                  = "img_framed";
  public final static String IMG_FRAMELESS               = "img_frameless";
  public final static String IMG_LANG                    = "img_lang";
  public final static String IMG_LEFT                    = "img_left";
  public final static String IMG_LINK                    = "img_link";
  public final static String IMG_LOSSY                   = "img_lossy";
  public final static String IMG_MANUAL_THUMB            = "img_manualthumb";
  public final static String IMG_MIDDLE                  = "img_middle";
  public final static String IMG_NONE                    = "img_none";
  public final static String IMG_PAGE                    = "img_page";
  public final static String IMG_RIGHT                   = "img_right";
  public final static String IMG_SUB                     = "img_sub";
  public final static String IMG_SUPER                   = "img_super";
  public final static String IMG_TEXT_BOTTOM             = "img_text_bottom";
  public final static String IMG_TEXT_TOP                = "img_text_top";
  public final static String IMG_THUMBNAIL               = "img_thumbnail";
  public final static String IMG_TOP                     = "img_top";
  public final static String IMG_UPRIGHT                 = "img_upright";
  public final static String IMG_WIDTH                   = "img_width";
  public final static String INDEX                       = "index";
  public final static String INFO_PAGE                   = "infopage";
  public final static String INT                         = "int";
  public final static String INVOKE                      = "invoke";
  public final static String LANGUAGE                    = "language";
  public final static String LC                          = "lc";
  public final static String LC_FIRST                    = "lcfirst";
  public final static String LEN                         = "len";
  public final static String LINK_UNIT                   = "linkunit";
  public final static String LOCAL_DAY                   = "localday";
  public final static String LOCAL_DAY_2                 = "localday2";
  public final static String LOCAL_DAY_NAME              = "localdayname";
  public final static String LOCAL_DOW                   = "localdow";
  public final static String LOCAL_HOUR                  = "localhour";
  public final static String LOCAL_MONTH                 = "localmonth";
  public final static String LOCAL_MONTH_1               = "localmonth1";
  public final static String LOCAL_MONTH_ABBREV          = "localmonthabbrev";
  public final static String LOCAL_MONTH_NAME            = "localmonthname";
  public final static String LOCAL_MONTH_NAME_GEN        = "localmonthnamegen";
  public final static String LOCAL_TIME                  = "localtime";
  public final static String LOCAL_TIMESTAMP             = "localtimestamp";
  public final static String LOCAL_URL                   = "localurl";
  public final static String LOCAL_URL_E                 = "localurle";
  public final static String LOCAL_WEEK                  = "localweek";
  public final static String LOCAL_YEAR                  = "localyear";
  public final static String LQT_PAGE_LIMIT              = "lqtpagelimit";
  public final static String MSG                         = "msg";
  public final static String MSGNW                       = "msgnw";
  public final static String NAMESPACE                   = "namespace";
  public final static String NAMESPACE_E                 = "namespacee";
  public final static String NAMESPACE_NUMBER            = "namespacenumber";
  public final static String NEW_SECTION_LINK            = "newsectionlink";
  public final static String NO_CONTENT_CONVERT          = "nocontentconvert";
  public final static String NO_EDIT_SECTION             = "noeditsection";
  public final static String NO_GALLERY                  = "nogallery";
  public final static String NO_HEADER                   = "noheader";
  public final static String NO_INDEX                    = "noindex";
  public final static String NO_NEW_SECTION_LINK         = "nonewsectionlink";
  public final static String NO_TITLE_CONVERT            = "notitleconvert";
  public final static String NO_TOC                      = "notoc";
  public final static String NS                          = "ns";
  public final static String NSE                         = "nse";
  public final static String NUMBER_IN_GROUP             = "numberingroup";
  public final static String NUMBER_OF_ACTIVE_USERS      = "numberofactiveusers";
  public final static String NUMBER_OF_ADMINS            = "numberofadmins";
  public final static String NUMBER_OF_ARTICLES          = "numberofarticles";
  public final static String NUMBER_OF_EDITS             = "numberofedits";
  public final static String NUMBER_OF_FILES             = "numberoffiles";
  public final static String NUMBER_OF_PAGES             = "numberofpages";
  public final static String NUMBER_OF_USERS             = "numberofusers";
  public final static String NUMBER_OF_VIEWS             = "numberofviews";
  public final static String OGG_NOICON                  = "ogg_noicon";
  public final static String OGG_NOPLAYER                = "ogg_noplayer";
  public final static String OGG_THUMBTIME               = "ogg_thumbtime";
  public final static String PAD_LEFT                    = "padleft";
  public final static String PAD_RIGHT                   = "padright";
  public final static String PAGE_ID                     = "pageid";
  public final static String PAGE_NAME                   = "pagename";
  public final static String PAGE_NAME_E                 = "pagenamee";
  public final static String PAGE_SIZE                   = "pagesize";
  public final static String PAGES_IN_CATEGORY           = "pagesincategory";
  public final static String PAGES_IN_CATEGORY_ALL       = "pagesincategory_all";
  public final static String PAGES_IN_CATEGORY_FILES     = "pagesincategory_files";
  public final static String PAGES_IN_CATEGORY_PAGES     = "pagesincategory_pages";
  public final static String PAGES_IN_CATEGORY_SUBCATS   = "pagesincategory_subcats";
  public final static String PAGES_IN_NAMESPACE          = "pagesinnamespace";
  public final static String PAGES_USING_PENDING_CHANGES = "pagesusingpendingchanges";
  public final static String PENDING_CHANGE_LEVEL        = "pendingchangelevel";
  public final static String PLURAL                      = "plural";
  public final static String POS                         = "pos";
  public final static String PROTECTION_LEVEL            = "protectionlevel";
  public final static String RAW                         = "raw";
  public final static String RAW_SUFFIX                  = "rawsuffix";
  public final static String REDIRECT                    = "redirect";
  public final static String REL_2_ABS                   = "rel2abs";
  public final static String REPLACE                     = "replace";
  public final static String REVISION_DAY                = "revisionday";
  public final static String REVISION_DAY_2              = "revisionday2";
  public final static String REVISION_ID                 = "revisionid";
  public final static String REVISION_MONTH              = "revisionmonth";
  public final static String REVISION_MONTH_1            = "revisionmonth1";
  public final static String REVISION_TIMESTAMP          = "revisiontimestamp";
  public final static String REVISION_USER               = "revisionuser";
  public final static String REVISION_YEAR               = "revisionyear";
  public final static String RPOS                        = "rpos";
  public final static String SAFE_SUBST                  = "safesubst";
  public final static String SCRIPT_PATH                 = "scriptpath";
  public final static String SERVER                      = "server";
  public final static String SERVER_NAME                 = "servername";
  public final static String SIGNIFICANT_FIGURES         = "significantfigures";
  public final static String SITE_NAME                   = "sitename";
  public final static String SOURCE_UNIT                 = "sourceunit";
  public final static String SPECIAL                     = "special";
  public final static String SPECIAL_E                   = "speciale";
  public final static String STATIC_REDIRECT             = "staticredirect";
  public final static String STYLE_PATH                  = "stylepath";
  public final static String SUB                         = "sub";
  public final static String SUB_PAGE_NAME               = "subpagename";
  public final static String SUB_PAGE_NAME_E             = "subpagenamee";
  public final static String SUBJECT_PAGE_NAME           = "subjectpagename";
  public final static String SUBJECT_PAGE_NAME_E         = "subjectpagenamee";
  public final static String SUBJECT_SPACE               = "subjectspace";
  public final static String SUBJECT_SPACE_E             = "subjectspacee";
  public final static String SUBST                       = "subst";
  public final static String SWITCH                      = "switch";
  public final static String SWITCH_COUNTRY              = "switchcountry";
  public final static String SWITCH_LANGUAGE             = "switchlanguage";
  public final static String TAG                         = "tag";
  public final static String TALK_PAGE_NAME              = "talkpagename";
  public final static String TALK_PAGE_NAME_E            = "talkpagenamee";
  public final static String TALK_SPACE                  = "talkspace";
  public final static String TALK_SPACE_E                = "talkspacee";
  public final static String TARGET_UNIT                 = "targetunit";
  public final static String TIME                        = "time";
  public final static String TIMED_MEDIA_ENDTIME         = "timedmedia_endtime";
  public final static String TIMED_MEDIA_NOICON          = "timedmedia_noicon";
  public final static String TIMED_MEDIA_NOPLAYER        = "timedmedia_noplayer";
  public final static String TIMED_MEDIA_STARTTIME       = "timedmedia_starttime";
  public final static String TIMED_MEDIA_THUMBTIME       = "timedmedia_thumbtime";
  public final static String TIME_L                      = "timel";
  public final static String TITLE_PARTS                 = "titleparts";
  public final static String TOC                         = "toc";
  public final static String TRANSLATION_DIALOG          = "translationdialog";
  public final static String UC                          = "uc";
  public final static String UC_FIRST                    = "ucfirst";
  public final static String URL_DECODE                  = "urldecode";
  public final static String URL_ENCODE                  = "urlencode";
  public final static String URL_PATH                    = "url_path";
  public final static String URL_QUERY                   = "url_query";
  public final static String URL_WIKI                    = "url_wiki";
  public final static String USE_LIQUID_THREADS          = "useliquidthreads";
  public final static String USER_TEST_WIKI              = "usertestwiki";

  /**
   * List of magic words that can be used as variables / functions.
   */
  private final static String[] functionMagicWords = {
    MagicWord.ANCHOR_ENCODE,
    MagicWord.BASE_PAGE_NAME,
    MagicWord.BASE_PAGE_NAME_E,
    MagicWord.CANONICAL_URL,
    MagicWord.CONTENT_LANGUAGE,
    MagicWord.CURRENT_DAY,
    MagicWord.CURRENT_DAY_2,
    MagicWord.CURRENT_DAY_NAME,
    MagicWord.CURRENT_DOW,
    MagicWord.CURRENT_HOUR,
    MagicWord.CURRENT_MONTH,
    MagicWord.CURRENT_MONTH_ABBREV,
    MagicWord.CURRENT_MONTH_NAME,
    MagicWord.CURRENT_MONTH_NAME_GEN,
    MagicWord.CURRENT_TIME,
    MagicWord.CURRENT_TIMESTAMP,
    MagicWord.CURRENT_VERSION,
    MagicWord.CURRENT_WEEK,
    MagicWord.CURRENT_YEAR,
    MagicWord.DEFAULT_SORT,
    MagicWord.DIRECTION_MARK,
    MagicWord.DISPLAY_TITLE,
    MagicWord.EXPLODE,
    MagicWord.EXPR,
    MagicWord.FILE_PATH,
    MagicWord.FORMAT_DATE,
    MagicWord.FORMAT_NUM,
    MagicWord.FULL_PAGE_NAME,
    MagicWord.FULL_PAGE_NAME_E,
    MagicWord.FULL_URL,
    MagicWord.GENDER,
    MagicWord.GRAMMAR,
    MagicWord.IF,
    MagicWord.IF_EQ,
    MagicWord.IF_ERROR,
    MagicWord.IF_EXIST,
    MagicWord.IF_EXPR,
    MagicWord.INT,
    MagicWord.INVOKE,
    MagicWord.LANGUAGE,
    MagicWord.LC,
    MagicWord.LC_FIRST,
    MagicWord.LEN,
    MagicWord.LOCAL_DAY,
    MagicWord.LOCAL_DAY_2,
    MagicWord.LOCAL_DAY_NAME,
    MagicWord.LOCAL_DOW,
    MagicWord.LOCAL_HOUR,
    MagicWord.LOCAL_MONTH,
    MagicWord.LOCAL_MONTH_ABBREV,
    MagicWord.LOCAL_MONTH_NAME,
    MagicWord.LOCAL_MONTH_NAME_GEN,
    MagicWord.LOCAL_TIME,
    MagicWord.LOCAL_TIMESTAMP,
    MagicWord.LOCAL_URL,
    MagicWord.LOCAL_WEEK,
    MagicWord.LOCAL_YEAR,
    MagicWord.NAMESPACE,
    MagicWord.NAMESPACE_E,
    MagicWord.NAMESPACE_NUMBER,
    MagicWord.NS,
    MagicWord.NUMBER_IN_GROUP,
    MagicWord.NUMBER_OF_ACTIVE_USERS,
    MagicWord.NUMBER_OF_ADMINS,
    MagicWord.NUMBER_OF_ARTICLES,
    MagicWord.NUMBER_OF_EDITS,
    MagicWord.NUMBER_OF_FILES,
    MagicWord.NUMBER_OF_PAGES,
    MagicWord.NUMBER_OF_USERS,
    MagicWord.NUMBER_OF_VIEWS,
    MagicWord.PAD_LEFT,
    MagicWord.PAD_RIGHT,
    MagicWord.PAGE_ID,
    MagicWord.PAGE_NAME,
    MagicWord.PAGE_NAME_E,
    MagicWord.PAGE_SIZE,
    MagicWord.PAGES_IN_CATEGORY,
    MagicWord.PAGES_IN_NAMESPACE,
    MagicWord.PLURAL,
    MagicWord.POS,
    MagicWord.PROTECTION_LEVEL,
    MagicWord.REL_2_ABS,
    MagicWord.REPLACE,
    MagicWord.REVISION_DAY,
    MagicWord.REVISION_DAY_2,
    MagicWord.REVISION_ID,
    MagicWord.REVISION_MONTH,
    MagicWord.REVISION_MONTH_1,
    MagicWord.REVISION_TIMESTAMP,
    MagicWord.REVISION_USER,
    MagicWord.REVISION_YEAR,
    MagicWord.RPOS,
    MagicWord.SCRIPT_PATH,
    MagicWord.SERVER,
    MagicWord.SERVER_NAME,
    MagicWord.SITE_NAME,
    MagicWord.SPECIAL,
    MagicWord.SPECIAL_E,
    MagicWord.STYLE_PATH,
    MagicWord.SAFE_SUBST,
    MagicWord.SUB,
    MagicWord.SUB_PAGE_NAME,
    MagicWord.SUB_PAGE_NAME_E,
    MagicWord.SUBJECT_PAGE_NAME,
    MagicWord.SUBJECT_PAGE_NAME_E,
    MagicWord.SUBJECT_SPACE,
    MagicWord.SUBJECT_SPACE_E,
    MagicWord.SUBST,
    MagicWord.SWITCH,
    MagicWord.TAG,
    MagicWord.TALK_PAGE_NAME,
    MagicWord.TALK_PAGE_NAME_E,
    MagicWord.TALK_SPACE,
    MagicWord.TALK_SPACE_E,
    MagicWord.TIME,
    MagicWord.TIME_L,
    MagicWord.TITLE_PARTS,
    MagicWord.UC,
    MagicWord.UC_FIRST,
    MagicWord.URL_DECODE,
    MagicWord.URL_ENCODE,
  };

  /**
   * List of magic words that can be used in images.
   */
  private final static String[] imgMagicWords = {
    MagicWord.IMG_ALT,
    MagicWord.IMG_BASELINE,
    MagicWord.IMG_BORDER,
    MagicWord.IMG_BOTTOM,
    MagicWord.IMG_CENTER,
    MagicWord.IMG_CLASS,
    MagicWord.IMG_FRAMED,
    MagicWord.IMG_FRAMELESS,
    MagicWord.IMG_LANG,
    MagicWord.IMG_LEFT,
    MagicWord.IMG_LINK,
    MagicWord.IMG_LOSSY,
    MagicWord.IMG_MANUAL_THUMB,
    MagicWord.IMG_MIDDLE,
    MagicWord.IMG_NONE,
    MagicWord.IMG_PAGE,
    MagicWord.IMG_RIGHT,
    MagicWord.IMG_SUB,
    MagicWord.IMG_SUPER,
    MagicWord.IMG_TEXT_BOTTOM,
    MagicWord.IMG_TEXT_TOP,
    MagicWord.IMG_THUMBNAIL,
    MagicWord.IMG_TOP,
    MagicWord.IMG_UPRIGHT,
    MagicWord.IMG_WIDTH,
  };

  /**
   * List of magic words that need a # to be used.
   */
  private final static String[] sharpMagicWords = {
    MagicWord.EXPLODE,
    MagicWord.EXPR,
    MagicWord.FORMAT_DATE,
    MagicWord.IF,
    MagicWord.IF_EQ,
    MagicWord.IF_ERROR,
    MagicWord.IF_EXIST,
    MagicWord.IF_EXPR,
    MagicWord.INVOKE,
    MagicWord.LANGUAGE,
    MagicWord.LEN,
    MagicWord.POS,
    MagicWord.REL_2_ABS,
    MagicWord.REPLACE,
    MagicWord.RPOS,
    MagicWord.SPECIAL,
    MagicWord.SPECIAL_E,
    MagicWord.SUB,
    MagicWord.SWITCH,
    MagicWord.TAG,
    MagicWord.TIME,
    MagicWord.TITLE_PARTS,
  };

  /**
   * @param name Magic word name.
   * @param aliases Magic word aliases.
   * @param caseSensitive True if case sensitiveness is needed.
   */
  public MagicWord(String name, List<String> aliases, boolean caseSensitive) {
    this.name = name;
    this.aliases = aliases;
    this.caseSensitive = caseSensitive;
  }

  /**
   * @return Magic word name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Magic word aliases.
   */
  public List<String> getAliases() {
    return aliases;
  }

  /**
   * @param text Text to check.
   * @return Flag indicating if the text is a possible alias.
   */
  public boolean isPossibleAlias(String text) {
    return isPossibleAlias(text, ".*");
  }

  /**
   * @param text Text to check.
   * @param pattern Pattern acceptable for replacing $1
   * @return Flag indicating if the text is a possible alias.
   */
  public boolean isPossibleAlias(String text, String pattern) {
    if (text == null) {
      return false;
    }
    if ((text.length() > 0) && (text.charAt(0) == '#')) {
      boolean sharp = false;
      for (String magicWord : sharpMagicWords) {
        if (magicWord.equals(name)) {
          sharp = true;
        }
      }
      if (sharp) {
        text = text.substring(1);
      }
    }
    for (String alias : aliases) {
      if (alias.contains("$1")) {
        if (text.matches(alias.replaceAll("\\$1", pattern))) {
          return true;
        }
      } else if (alias.equals(text)) {
        return true;
      } else if (!caseSensitive && alias.equalsIgnoreCase(text)) {
        return true;
      }
    }
    return false;
  }

  /**
   * @return List of magic words that can be used as functions.
   */
  public static List<String> getFunctionMagicWords() {
    List<String> result = new ArrayList<String>(functionMagicWords.length);
    for (String magicWord : functionMagicWords) {
      result.add(magicWord);
    }
    return result;
  }

  /**
   * @return List of magic words that can be used in images.
   */
  public static List<String> getImgMagicWords() {
    List<String> result = new ArrayList<String>(imgMagicWords.length);
    for (String magicWord : imgMagicWords) {
      result.add(magicWord);
    }
    return result;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(MagicWord mw) {
    int compare;

    // Name
    compare = name.compareTo(mw.name);
    if (compare != 0) {
      return compare;
    }

    return compare;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if ((o == null) || (o.getClass() != getClass())) {
      return false;
    }
    MagicWord mw = (MagicWord) o;
    boolean equals = true;
    equals &= name.equals(mw.name);
    return equals;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return name.hashCode();
  }
}
