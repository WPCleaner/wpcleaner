/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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
 * Encapsulate possible query results.
 */
public enum EnumQueryResult {

  ARTICLE_EXISTS       ("ArticleExists",
                        "The article you tried to create has been created already"),
  AUTO_BLOCKED         ("AutoBlocked",
                        "Your IP address has been blocked automatically, because it was used by a blocked user"),
  BAD_TOKEN            ("BadToken",
                        "Invalid token"),
  BLOCKED              ("Blocked",
                        "You have been blocked from editing"),
  CANT_CREATE          ("CantCreate",
                        "You don't have permission to create new pages"),
  CANT_CREATE_ANON     ("CantCreate-Anon",
                        "Anonymous users can't create new pages"),
  CANT_MOVE            ("CantMove",
                        "You don't have permission to move pages"),
  CANT_MOVE_FILE       ("CantMoveFile",
                        "You don't have permission to move files "),
  CANT_MOVE_ANON       ("CantMove-Anon",
                        "Anonymous users can't move pages"),
  CASCADE_PROTECTED    ("CascadeProtected",
                        "The page you're trying to edit is protected because it's included in a cascade-protected page"),
  CONFIRM_EMAIL        ("ConfirmEmail",
                        "You must confirm your e-mail address before you can edit"),
  CONTENT_TOO_BIG      ("ContentTooBig",
                        "The content you supplied exceeds the article size limit"),
  CUSTOM_CSS_JS        ("CustomCssJsProtected",
                        "You're not allowed to edit custom CSS and JavaScript pages"),
  EDIT_CONFLICT        ("EditConflict",
                        "Edit conflict detected"),
  EMPTY_NEW_SECTION    ("EmptyNewSection",
                        "Creating empty new sections is not possible"),
  EMPTY_PAGE           ("EmptyPage",
                        "Creating new, empty pages is not allowed"),
  FILTERED             ("Filtered",
                        "The filter callback function refused your edit"),
  HOOK_ABORTED         ("HookAborted",
                        "The modification you tried to make was aborted by an extension hook"),
  IMMOBILE_NAMESPACE   ("ImmobileNamespace",
                        "You tried to move pages from or to a namespace that is protected from moving"),
  INVALID_SECTION      ("InvalidSection",
                        "The section parameter must be set to an integer or 'new'"),
  INVALID_TITLE        ("InvalidTitle",
                        "Bad title"),
  INVALID_USER         ("InvalidUser",
                        "Invalid username"),
  MISSING_TITLE        ("MissingTitle",
                        "The page you requested doesn't exist"),
  MUST_BE_POSTED       ("MustBePosted",
                        "Type of your HTTP request message must be POST"),
  NO_API_WRITE         ("NoApiWrite",
                        "Editing of this wiki through the API is disabled"),
  NO_EDIT              ("NoEdit",
                        "You don't have permission to edit pages"),
  NO_EDIT_ANON         ("NoEdit-Anon",
                        "Anonymous users can't edit pages"),
  NO_FROM              ("NoFrom",
                        "The from parameter must be set"),
  NO_IMAGE_REDIR       ("NoImageRedirect",
                        "You don't have permission to create image redirects"),
  NO_IMAGE_REDIR_ANON  ("NoImageRedirect-Anon",
                        "Anonymous users can't create image redirects"),
  NO_SUCH_PAGE_ID      ("NoSuchPageId",
                        "There is no page with ID"),
  NO_SUCH_RC_ID        ("NoSuchRcId",
                        "There is no change with rcid"),
  NO_SUCH_REV_ID       ("NoSuchRevId",
                        "There is no revision with ID"),
  NO_SUCH_USER         ("NoSuchUser",
                        "The user you specified doesn't exist"),
  NO_SUPPRESS          ("NoSuppress",
                        "You don't have permission to suppress redirect creation"),
  NO_TEXT              ("NoText",
                        "The text parameter must be set"),
  NO_TITLE             ("NoTitle",
                        "The title parameter must be set"),
  NO_TO                ("NoTo",
                        "The to parameter must be set"),
  NO_TOKEN             ("NoToken",
                        "The token parameter must be set"),
  PAGE_DELETED         ("PageDeleted",
                        "The page has been deleted since you fetched its timestamp"),
  PERMISSION_DENIED    ("PermissionDenied",
                        "Permission denied"),
  PROTECTED_NAMESPACE  ("ProtectedNamespace",
                        "You're not allowed to edit pages in this namespace"),
  PROTECTED_NAMESPACE_I("ProtectedNamespace-interface",
                        "You're not allowed to edit interface messages"),
  PROTECTED_PAGE       ("ProtectedPage",
                        "The right is required to edit this page"),
  PROTECTED_TITLE      ("ProtectedTitle",
                        "This title has been protected from creation"),
  RATE_LIMITED         ("RateLimited",
                        "You've exceeded your rate limit. Please wait some time and try again"),
  READ_ONLY            ("ReadOnly",
                        "The wiki is currently in read-only mode"),
  REV_WRONG_PAGE       ("RevWrongPage",
                        "rrevid is not a revision of pagename"),
  RV_NO_SUCH_SECTION   ("RvNoSuchSection",
                        "There is no section section in rrevid"),
  SELF_MOVE            ("SelfMove",
                        "Can't move a page to itself"),
  SPAM_DETECTED        ("SpamDetected",
                        "Your edit was refused because it contained a spam fragment"),
  SUCCESS              (null,
                        "Query successful"),
  UNDO_FAILURE         ("UndoFailure",
                        "Undo failed due to conflicting intermediate edits"),
  UNSUPPORTED_NAMESPACE("UnsupportedNamespace",
                        "Pages in the Special namespace can't be edited"),
  WRITE_API_DENIED     ("WriteApiDenied",
                        "You're not allowed to edit this wiki through the API"),
  
  UNKNOWN_ERROR        ("UnknownError",
                        "Unknown error");

  private final String code;
  private final String text;

  /**
   * @param text Associated text
   */
  EnumQueryResult(String code, String text) {
    this.code = code;
    this.text = text;
  }

  /**
   * @param code Error code.
   * @return Matching EnumLoginResult.
   */
  public static EnumQueryResult getEnumByCode(String code) {
    for (EnumQueryResult result : EnumQueryResult.values()) {
      if ((result != null) &&
          (result.code != null) &&
          (result.code.equalsIgnoreCase(code))) {
        return result;
      }
    }
    return UNKNOWN_ERROR;
  }

  /**
   * @return Query successful ?
   */
  public boolean isOk() {
    return equals(SUCCESS);
  }

  /**
   * @return Associated text.
   */
  public String getText() {
    return text;
  }
}
