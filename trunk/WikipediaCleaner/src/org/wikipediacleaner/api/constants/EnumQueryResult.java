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
  EMPTY_PAGE           ("EmptyPage",
                        "Creating new, empty pages is not allowed"),
  FILTERED             ("Filtered",
                        "The filter callback function refused your edit"),
  HOOK_ABORTED         ("HookAborted",
                        "The modification you tried to make was aborted by an extension hook"),
  INVALID_SECTION      ("InvalidSection",
                        "The section parameter must be set to an integer or 'new'"),
  INVALID_TITLE        ("InvalidTitle",
                        "Bad title"),
  INVALID_USER         ("InvalidUser",
                        "Invalid username"),
  MISSING_TITLE        ("MissingTitle",
                        "The page you requested doesn't exist"),
  NO_EDIT              ("NoEdit",
                        "You don't have permission to edit pages"),
  NO_EDIT_ANON         ("NoEdit-Anon",
                        "Anonymous users can't edit pages"),
  NO_IMAGE_REDIR       ("NoImageRedirect",
                        "You don't have permission to create image redirects"),
  NO_IMAGE_REDIR_ANON  ("NoImageRedirect-Anon",
                        "Anonymous users can't create image redirects"),
  NO_SUCH_USER         ("NoSuchUser",
                        "The user you specified doesn't exist"),
  NO_TEXT              ("NoText",
                        "The text parameter must be set"),
  NO_TITLE             ("NoTitle",
                        "The title parameter must be set"),
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
  SPAM_DETECTED        ("SpamDetected",
                        "Your edit was refused because it contained a spam fragment"),
  SUCCESS              (null,
                        "Query successful"),
  UNSUPPORTED_NAMESPACE("UnsupportedNamespace",
                        "Pages in the Special namespace can't be edited"),
  
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
