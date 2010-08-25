/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

package org.wikipediacleaner.api.data;

import java.util.List;


/**
 * Informations about magic words.
 */
public class MagicWord implements Comparable<MagicWord> {

  private final String name;
  private final List<String> aliases;

  public final static String ANCHOR_ENCODE          = "anchorencode";
  public final static String BASE_PAGE_NAME         = "basepagename";
  public final static String BASE_PAGE_NAME_E       = "basepagnenamee";
  public final static String CATEGORY_TREE          = "categorytree";
  public final static String CONTENT_LANGUAGE       = "contentlanguage";
  public final static String COUNT                  = "count";
  public final static String CURRENT_DAY            = "currentday";
  public final static String CURRENT_DAY_2          = "currentday2";
  public final static String CURRENT_DAY_NAME       = "currentdayname";
  public final static String CURRENT_DOW            = "currentdow";
  public final static String CURRENT_HOUR           = "currenthour";
  public final static String CURRENT_MONTH          = "currentmonth";
  public final static String CURRENT_MONTH_1        = "currentmonth1";
  public final static String CURRENT_MONTH_ABBREV   = "currentmonthabbrev";
  public final static String CURRENT_MONTH_NAME     = "currentmonthname";
  public final static String CURRENT_MONTH_NAME_GEN = "currentmonthnamegen";
  public final static String CURRENT_TIME           = "currenttime";
  public final static String CURRENT_TIMESTAMP      = "currenttimestamp";
  public final static String CURRENT_VERSION        = "currentversion";
  public final static String CURRENT_WEEK           = "currentweek";
  public final static String CURRENT_YEAR           = "currentyear";
  public final static String DEFAULT                = "default";
  public final static String DEFAULT_SORT           = "defaultsort";
  public final static String DIRECTION_MARK         = "directionmark";
  public final static String DISPLAY_TITLE          = "displaytitle";
  public final static String EXPLODE                = "explode";
  public final static String EXPR                   = "expr";
  public final static String FILE_PATH              = "filepath";
  public final static String FORCE_TOC              = "forcetoc";
  public final static String FORMAT_DATE            = "formatdate";
  public final static String FORMAT_NUM             = "formatnum";
  public final static String FULL_PAGE_NAME         = "fullpagename";
  public final static String FULL_PAGE_NAME_E       = "fullpagenamee";
  public final static String FULL_URL               = "fullurl";
  public final static String FULL_URL_E             = "fullurle";
  public final static String GENDER                 = "gender";
  public final static String GRAMMAR                = "grammar";
  public final static String HIDDEN_CAT             = "hiddencat";
  public final static String IF                     = "if";
  public final static String IF_EQ                  = "ifeq";
  public final static String IF_ERROR               = "iferror";
  public final static String IF_EXIST               = "ifexist";
  public final static String IF_EXPR                = "ifexpr";
  public final static String INT                    = "int";
  public final static String IMG_ALT                = "img_alt";
  public final static String IMG_BASELINE           = "img_baseline";
  public final static String IMG_BORDER             = "img_border";
  public final static String IMG_BOTTOM             = "img_bottom";
  public final static String IMG_CENTER             = "img_center";
  public final static String IMG_FRAMED             = "img_framed";
  public final static String IMG_FRAMELESS          = "img_frameless";
  public final static String IMG_LEFT               = "img_left";
  public final static String IMG_LINK               = "img_link";
  public final static String IMG_MANUAL_THUMB       = "img_manualthumb";
  public final static String IMG_MIDDLE             = "img_middle";
  public final static String IMG_NONE               = "img_none";
  public final static String IMG_PAGE               = "img_page";
  public final static String IMG_RIGHT              = "img_right";
  public final static String IMG_SUB                = "img_sub";
  public final static String IMG_SUPER              = "img_super";
  public final static String IMG_TEXT_BOTTOM        = "img_text_bottom";
  public final static String IMG_TEXT_TOP           = "img_text_top";
  public final static String IMG_THUMBNAIL          = "img_thumbnail";
  public final static String IMG_TOP                = "img_top";
  public final static String IMG_UPRIGHT            = "img_upright";
  public final static String IMG_WIDTH              = "img_width";
  public final static String INDEX                  = "index";
  public final static String LANGUAGE               = "language";
  public final static String LC                     = "lc";
  public final static String LC_FIRST               = "lcfirst";
  public final static String LEN                    = "len";
  public final static String LOCAL_DAY              = "localday";
  public final static String LOCAL_DAY_2            = "localday2";
  public final static String LOCAL_DAY_NAME         = "localdayname";
  public final static String LOCAL_DOW              = "localdow";
  public final static String LOCAL_HOUR             = "localhour";
  public final static String LOCAL_MONTH            = "localmonth";
  public final static String LOCAL_MONTH_1          = "localmonth1";
  public final static String LOCAL_MONTH_ABBREV     = "localmonthabbrev";
  public final static String LOCAL_MONTH_NAME       = "localmonthname";
  public final static String LOCAL_MONTH_NAME_GEN   = "localmonthnamegen";
  public final static String LOCAL_TIME             = "localtime";
  public final static String LOCAL_TIMESTAMP        = "localtimestamp";
  public final static String LOCAL_URL              = "localurl";
  public final static String LOCAL_URL_E            = "localurle";
  public final static String LOCAL_WEEK             = "localweek";
  public final static String LOCAL_YEAR             = "localyear";
  public final static String LQT_PAGE_LIMIT         = "lqtpagelimit";
  public final static String MSG                    = "msg";
  public final static String MSGNW                  = "msgnw";
  public final static String NAMESPACE              = "namespace";
  public final static String NAMESPACE_E            = "namespacee";
  public final static String NEW_SECTION_LINK       = "newsectionlink";
  public final static String NO_CONTENT_CONVERT     = "nocontentconvert";
  public final static String NO_EDIT_SECTION        = "noeditsection";
  public final static String NO_GALLERY             = "nogallery";
  public final static String NO_HEADER              = "noheader";
  public final static String NO_INDEX               = "noindex";
  public final static String NO_NEW_SECTION_LINK    = "nonewsectionlink";
  public final static String NO_TITLE_CONVERT       = "notitleconvert";
  public final static String NO_TOC                 = "notoc";
  public final static String NS                     = "ns";
  public final static String NSE                    = "nse";
  public final static String NUMBER_IN_GROUP        = "numberingroup";
  public final static String NUMBER_OF_ACTIVE_USERS = "numberofactiveusers";
  public final static String NUMBER_OF_ADMINS       = "numberofadmins";
  public final static String NUMBER_OF_ARTICLES     = "numberofarticles";
  public final static String NUMBER_OF_EDITS        = "numberofedits";
  public final static String NUMBER_OF_FILES        = "numberoffiles";
  public final static String NUMBER_OF_PAGES        = "numberofpages";
  public final static String NUMBER_OF_USERS        = "numberofusers";
  public final static String NUMBER_OF_VIEWS        = "numberofviews";
  public final static String OGG_NOICON             = "ogg_noicon";
  public final static String OGG_NOPLAYER           = "ogg_noplayer";
  public final static String OGG_THUMBTIME          = "ogg_thumbtime";
  public final static String PAD_LEFT               = "padleft";
  public final static String PAD_RIGHT              = "padright";
  public final static String PAGE_NAME              = "pagename";
  public final static String PAGE_NAME_E            = "pagenamee";
  public final static String PAGE_SIZE              = "pagesize";
  public final static String PAGES_IN_CATEGORY      = "pagesincategory";
  public final static String PAGES_IN_NAMESPACE     = "pagesinnamespace";
  public final static String PLURAL                 = "plural";
  public final static String POS                    = "pos";
  public final static String PROTECTION_LEVEL       = "protectionlevel";
  public final static String RAW                    = "raw";
  public final static String RAW_SUFFIX             = "rawsuffix";
  public final static String REDIRECT               = "redirect";
  public final static String REL_2_ABS              = "rel2abs";
  public final static String REPLACE                = "replace";
  public final static String REVISION_DAY           = "revisionday";
  public final static String REVISION_DAY_2         = "revisionday2";
  public final static String REVISION_ID            = "revisionid";
  public final static String REVISION_MONT          = "revisionmonth";
  public final static String REVISION_TIMESTAMP     = "revisiontimestamp";
  public final static String REVISION_USER          = "revisionuser";
  public final static String REVISION_YEAR          = "revisionyear";
  public final static String RPOS                   = "rpos";
  public final static String SAFE_SUBST             = "safesubst";
  public final static String SCRIPT_PATH            = "scriptpath";
  public final static String SERVER                 = "server";
  public final static String SERVER_NAME            = "servername";
  public final static String SITE_NAME              = "sitename";
  public final static String SPECIAL                = "special";
  public final static String STATIC_REDIRECT        = "staticredirect";
  public final static String STYLE_PATH             = "stylepath";
  public final static String SUB                    = "sub";
  public final static String SUB_PAGE_NAME          = "subpagename";
  public final static String SUB_PAGE_NAME_E        = "subpagenamee";
  public final static String SUBJECT_PAGE_NAME      = "subjectpagename";
  public final static String SUBJECT_PAGE_NAME_E    = "subjectpagenamee";
  public final static String SUBJECT_SPACE          = "subjectspace";
  public final static String SUBJECT_SPACE_E        = "subjectspacee";
  public final static String SUBST                  = "subst";
  public final static String SWITCH                 = "switch";
  public final static String TAG                    = "tag";
  public final static String TALK_PAGE_NAME         = "talkpagename";
  public final static String TALK_PAGE_NAME_E       = "talkpagenamee";
  public final static String TALK_SPACE             = "talkspace";
  public final static String TALK_SPACE_E           = "talkspacee";
  public final static String TIME                   = "time";
  public final static String TIME_L                 = "timel";
  public final static String TITLE_PARTS            = "titleparts";
  public final static String TOC                    = "toc";
  public final static String UC                     = "uc";
  public final static String UC_FIRST               = "ucfirst";
  public final static String URL_ENCODE             = "urlencode";
  public final static String USE_LIQUID_THREADS     = "useliquidthreads";

  /**
   * @param name Magic word name.
   * @param aliases Magic word aliases.
   */
  public MagicWord(String name, List<String> aliases) {
    this.name = name;
    this.aliases = aliases;
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
    for (String alias : aliases) {
      if (alias.contains("$1")) {
        if (text.matches(alias.replaceAll("\\$1", pattern))) {
          return true;
        }
      } else if (alias.equals(text)) {
        return true;
      }
    }
    return false;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
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
    int hash = 7;
    hash = 31 * hash + name.hashCode();
    return hash;
  }
}
