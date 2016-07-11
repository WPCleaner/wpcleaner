/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;


/**
 * Information about special pages.
 */
public class SpecialPage implements Comparable<SpecialPage> {

  /**
   * Special page name.
   */
  private final String name;

  /**
   * List of special page aliases.
   */
  private final List<String> aliases;

  public final static String ABUSE_FILTER              = "AbuseFilter";
  public final static String ABUSE_LOG                 = "AbuseLog";
  public final static String ACTIVE_USERS              = "Activeusers";
  public final static String ALL_MESSAGES              = "Allmessages";
  public final static String ALL_MY_UPLOADS            = "AllMyUploads";
  public final static String ALL_PAGES                 = "Allpages";
  public final static String ANCIENT_PAGES             = "Ancientpages";
  public final static String API_FEATURE_USAGE         = "ApiFeatureUsage";
  public final static String API_HELP                  = "ApiHelp";
  public final static String API_SANDBOX               = "ApiSandbox";
  public final static String BANNER_LOADER             = "BannerLoader";
  public final static String BANNER_RANDOM             = "BannerRandom";
  public final static String BETA_FEATURES             = "BetaFeatures";
  public final static String BLANK_PAGE                = "Blankpage";
  public final static String BLOCK                     = "Block";
  public final static String BLOCK_LIST                = "BlockList";
  public final static String BOOK                      = "Book";
  public final static String BOOKSOURCES               = "Booksources";
  public final static String BOT_PASSWORDS             = "BotPasswords";
  public final static String BROKEN_REDIRECTS          = "BrokenRedirects";
  public final static String CAPTCHA                   = "Captcha";
  public final static String CATEGORIES                = "Categories";
  public final static String CATEGORY_TREE             = "CategoryTree";
  public final static String CENTRAL_AUTH              = "CentralAuth";
  public final static String CENTRAL_AUTO_LOGIN        = "CentralAutoLogin";
  public final static String CENTRAL_LOGIN             = "CentralLogin";
  public final static String CHANGE_CONTENT_MODEL      = "ChangeContentModel";
  public final static String CHANGE_CREDENTIALS        = "ChangeCredentials";
  public final static String CHANGE_EMAIL              = "ChangeEmail";
  public final static String CHANGE_PASSWORD           = "ChangePassword";
  public final static String CHECK_USER                = "CheckUser";
  public final static String CHECK_USER_LOG            = "CheckUserLog";
  public final static String CITE_THIS_PAGE            = "CiteThisPage";
  public final static String CN_REPORTER               = "CNReporter";
  public final static String COMPARE_PAGES             = "ComparePages";
  public final static String CONFIRM_EMAIL             = "Confirmemail";
  public final static String CONTENT_TRANSLATION       = "ContentTranslation";
  public final static String CONTENT_TRANSLATION_STATS = "ContentTranslationStats";
  public final static String CONTRIBUTIONS             = "Contributions";
  public final static String CREATE_ACCOUNT            = "CreateAccount";
  public final static String CREATE_MASS_MESSAGE_LIST  = "CreateMassMessageList";
  public final static String DEAD_END_PAGES            = "Deadendpages";
  public final static String DELETED_CONTRIBUTIONS     = "DeletedContributions";
  public final static String DIFF                      = "Diff";
  public final static String DISAMBIGUATION_PAGE_LINKS = "DisambiguationPageLinks";
  public final static String DISAMBIGUATION_PAGES      = "DisambiguationPages";
  public final static String DISPLAY_NOTIFICATIONS_CONFIGURATION = "DisplayNotificationsConfiguration";
  public final static String DOUBLE_REDIRECTS          = "DoubleRedirects";
  public final static String EDIT_MASS_MESSAGE_LIST    = "EditMassMessageList";
  public final static String EDIT_TAGS                 = "EditTags";
  public final static String EDIT_WATCHLIST            = "EditWatchlist";
  public final static String EMAIL_USER                = "Emailuser";
  public final static String ENABLE_FLOW               = "EnableFlow";
  public final static String EXPAND_TEMPLATES          = "ExpandTemplates";
  public final static String EXPORT                    = "Export";
  public final static String FEED_ITEM                 = "FeedItem";
  public final static String FEWEST_REVISIONS          = "Fewestrevisions";
  public final static String FILE_DUPLICATE_SEARCH     = "FileDuplicateSearch";
  public final static String FILE_PATH                 = "Filepath";
  public final static String FLOW                      = "Flow";
  public final static String GADGET_USAGE              = "GadgetUsage";
  public final static String GADGETS                   = "Gadgets";
  public final static String GLOBAL_BLOCK              = "GlobalBlock";
  public final static String GLOBAL_BLOCK_LIST         = "GlobalBlockList";
  public final static String GLOBAL_BLOCK_STATUS       = "GlobalBlockStatus";
  public final static String GLOBAL_GROUP_MEMBERSHIP   = "GlobalGroupMembership";
  public final static String GLOBAL_GROUP_PERMISSIONS  = "GlobalGroupPermissions";
  public final static String GLOBAL_RENAME_PROGRESS    = "GlobalRenameProgress";
  public final static String GLOBAL_RENAME_QUEUE       = "GlobalRenameQueue";
  public final static String GLOBAL_RENAME_REQUEST     = "GlobalRenameRequest";
  public final static String GLOBAL_RENAME_USER        = "GlobalRenameUser";
  public final static String GLOBAL_USAGE              = "GlobalUsage";
  public final static String GLOBAL_USER_MERGE         = "GlobalUserMerge";
  public final static String GLOBAL_USERS              = "GlobalUsers";
  public final static String GLOBALLY_WANTED_FILES     = "GloballyWantedFiles";
  public final static String GRAPH_SANDBOX             = "GraphSandbox";
  public final static String HIDE_BANNERS              = "HideBanners";
  public final static String HIEROGLYPHS               = "Hieroglyphs";
  public final static String HISTORY                   = "History";
  public final static String IMPORT                    = "Import";
  public final static String INTERWIKI                 = "Interwiki";
  public final static String INVALIDATE_EMAIL          = "Invalidateemail";
  public final static String LINK_ACCOUNTS             = "LinkAccounts";
  public final static String LINK_SEARCH               = "LinkSearch";
  public final static String LIST_ADMINS               = "Listadmins";
  public final static String LIST_BOTS                 = "Listbots";
  public final static String LIST_DUPLICATED_FILES     = "ListDuplicatedFiles";
  public final static String LIST_FILES                = "Listfiles";
  public final static String LIST_GRANTS               = "Listgrants";
  public final static String LIST_GROUP_RIGHTS         = "Listgrouprights";
  public final static String LIST_REDIRECTS            = "Listredirects";
  public final static String LIST_USERS                = "Listusers";
  public final static String LOCK_DB                   = "Lockdb";
  public final static String LOG                       = "Log";
  public final static String LONELY_PAGES              = "Lonelypages";
  public final static String LONG_PAGES                = "Longpages";
  public final static String MASS_MESSAGE              = "MassMessage";
  public final static String MATH_SHOW_IMAGE           = "MathShowImage";
  public final static String MATH_STATUS               = "MathStatus";
  public final static String MEDIA_STATISTICS          = "MediaStatistics";
  public final static String MERGE_ACCOUNT             = "MergeAccount";
  public final static String MERGE_HISTORY             = "MergeHistory";
  public final static String MIME_SEARCH               = "MIMEsearch";
  public final static String MOBILE_CITE               = "MobileCite";
  public final static String MOBILE_DIFF               = "MobileDiff";
  public final static String MOBILE_EDITOR             = "MobileEditor";
  public final static String MOBILE_LANGUAGES          = "MobileLanguages";
  public final static String MOBILE_MENU               = "MobileMenu";
  public final static String MOBILE_OPTIONS            = "MobileOptions";
  public final static String MOST_CATEGORIES           = "Mostcategories";
  public final static String MOST_GLOBALLY_LINKED_FILES = "MostGloballyLinkedFiles";
  public final static String MOST_IMAGES               = "Mostimages";
  public final static String MOST_INTERWIKIS           = "Mostinterwikis";
  public final static String MOST_LINKED               = "Mostlinked";
  public final static String MOST_LINKED_CATEGORIES    = "Mostlinkedcategories";
  public final static String MOST_LINKED_TEMPLATES     = "Mostlinkedtemplates";
  public final static String MOST_REVISIONS            = "Mostrevisions";
  public final static String MOVE_PAGE                 = "Movepage";
  public final static String MULTI_LOCK                = "MultiLock";
  public final static String MY_CONTRIBUTIONS          = "Mycontributions";
  public final static String MY_LANGUAGE               = "Mylanguage";
  public final static String MY_PAGE                   = "Mypage";
  public final static String MY_TALK                   = "Mytalk";
  public final static String MY_UPLOADS                = "Myuploads";
  public final static String NEARBY                    = "Nearby";
  public final static String NEW_IMAGES                = "Newimages";
  public final static String NEW_PAGES                 = "Newpages";
  public final static String NOTIFICATIONS             = "Notifications";
  public final static String NOTIFICATIONS_MARK_READ   = "NotificationsMarkRead";
  public final static String NUKE                      = "Nuke";
  public final static String OATH                      = "OATH";
  public final static String OAUTH                     = "OAuth";
  public final static String OAUTH_LIST_CONSUMERS      = "OAuthListConsumers";
  public final static String OAUTH_MANAGE_MY_GRANTS    = "OAuthManageMyGrants";
  public final static String ORPHANED_TIMED_TEXT       = "OrphanedTimedText";
  public final static String PAGES_WITH_BADGES         = "PagesWithBadges";
  public final static String PAGES_WITH_PROP           = "PagesWithProp";
  public final static String PASSWORD_RESET            = "PasswordReset";
  public final static String PERMANENT_LINK            = "PermanentLink";
  public final static String PREFERENCES               = "Preferences";
  public final static String PREFIX_INDEX              = "Prefixindex";
  public final static String PROTECTED_PAGES           = "Protectedpages";
  public final static String PROTECTED_TITLES          = "Protectedtitles";
  public final static String RANDOM_IN_CATEGORY        = "RandomInCategory";
  public final static String RANDOM_PAGE               = "Randompage";
  public final static String RANDOM_REDIRECT           = "Randomredirect";
  public final static String RANDOM_ROOT_PAGE          = "Randomrootpage";
  public final static String RECENT_CHANGES            = "Recentchanges";
  public final static String RECENT_CHANGES_LINKED     = "Recentchangeslinked";
  public final static String RECORD_IMPRESSION         = "RecordImpression";
  public final static String REDIRECT                  = "Redirect";
  public final static String REMOVE_CREDENTIALS        = "RemoveCredentials";
  public final static String REMOVE_GLOBAL_BLOCK       = "RemoveGlobalBlock";
  public final static String RENAME_USER               = "Renameuser";
  public final static String RESET_TOKENS              = "ResetTokens";
  public final static String REVISION_DELETE           = "Revisiondelete";
  public final static String RUN_JOBS                  = "RunJobs";
  public final static String SEARCH                    = "Search";
  public final static String SECURE_POLL               = "SecurePoll";
  public final static String SHORT_PAGES               = "Shortpages";
  public final static String SITE_MATRIX               = "SiteMatrix";
  public final static String SPECIAL_PAGES             = "Specialpages";
  public final static String STASTISTICS               = "Statistics";
  public final static String TAGS                      = "Tags";
  public final static String TEMPLATE_SANDBOX          = "TemplateSandbox";
  public final static String THANKS                    = "Thanks";
  public final static String TIMED_MEDIA_HANDLER       = "TimedMediaHandler";
  public final static String TRACKING_CATEGORIES       = "TrackingCategories";
  public final static String UNBLOCK                   = "Unblock";
  public final static String UNCATEGORIZED_CATEGORIES  = "Uncategorizedcategories";
  public final static String UNCATEGORIZED_IMAGES      = "Uncategorizedimages";
  public final static String UNCATEGORIZED_PAGES       = "Uncategorizedpages";
  public final static String UNCATEGORIZED_TEMPLATES   = "Uncategorizedtemplates";
  public final static String UNCONNECTED_PAGES         = "UnconnectedPages";
  public final static String UNDELETE                  = "Undelete";
  public final static String UNLINK_ACCOUNTS           = "UnlinkAccounts";
  public final static String UNLOCK_DB                 = "Unlockdb";
  public final static String UNUSED_CATEGORIES         = "Unusedcategories";
  public final static String UNUSED_IMAGES             = "Unusedimages";
  public final static String UNUSED_TEMPLATES          = "Unusedtemplates";
  public final static String UNWATCHED_PAGES           = "Unwatchedpages";
  public final static String UPLOAD                    = "Upload";
  public final static String UPLOAD_STASH              = "UploadStash";
  public final static String UPLOADS                   = "Uploads";
  public final static String URL_REDIRECTOR            = "UrlRedirector";
  public final static String URL_SHORTENER             = "UrlShortener";
  public final static String USER_LOGIN                = "Userlogin";
  public final static String USER_LOGOUT               = "Userlogout";
  public final static String USER_MERGE                = "UserMerge";
  public final static String USER_RIGHTS               = "Userrights";
  public final static String USERS_WHO_WILL_BE_RENAMED = "UsersWhoWillBeRenamed";
  public final static String VERSION                   = "Version";
  public final static String VIPS_TEST                 = "VipsTest";
  public final static String WANTED_CATEGORIES         = "Wantedcategories";
  public final static String WANTED_FILES              = "Wantedfiles";
  public final static String WANTED_PAGES              = "Wantedpages";
  public final static String WANTED_TEMPLATES          = "Wantedtemplates";
  public final static String WATCHLIST                 = "Watchlist";
  public final static String WHAT_LINKS_HERE           = "Whatlinkshere";
  public final static String WIKI_SETS                 = "WikiSets";
  public final static String WITHOUT_INTERWIKI         = "Withoutinterwiki";
  public final static String ZERO_RATED_MOBILE_ACCESS  = "ZeroRatedMobileAccess";

  /**
   * @param name Special page name.
   * @param aliases Special page aliases.
   */
  public SpecialPage(String name, List<String> aliases) {
    this.name = name;
    this.aliases = aliases;
  }

  /**
   * @return Special page name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Special page aliases.
   */
  public List<String> getAliases() {
    return aliases;
  }

  /**
   * @param text Text to check.
   * @return Flag indicating if the text is a possible alias.
   */
  public boolean isPossibleAlias(String text) {
    if (text == null) {
      return false;
    }
    for (String alias : aliases) {
      if (Page.areSameTitle(alias, text)) {
        return true;
      }
    }
    return false;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(SpecialPage sp) {
    int compare;

    // Name
    compare = name.compareTo(sp.name);
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
    SpecialPage sp = (SpecialPage) o;
    boolean equals = true;
    equals &= name.equals(sp.name);
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
