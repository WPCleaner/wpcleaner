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

package org.wikipediacleaner.api.base;


/**
 * Generic API Exception.
 */
public class APIException extends Exception {

  public static final String ERROR_AUTO_BLOCKED = "autoblocked";
  public static final String ERROR_BAD_TOKEN = "badtoken";
  public static final String ERROR_BLOCKED = "blocked";
  public static final String ERROR_CASCADE_PROTECTED = "cascadeprotected";
  public static final String ERROR_CONFIRM_EMAIL = "confirmemail";
  public static final String ERROR_CUSTOM_CSS_JS_PROTECTED = "customcssjsprotected";
  public static final String ERROR_HOOK_ABORTED = "hookaborted";
  public static final String ERROR_INVALID_TITLE = "invalidtitle";
  public static final String ERROR_INVALID_USER = "invaliduser";
  public static final String ERROR_MISSING_TITLE = "missingtitle";
  public static final String ERROR_MUST_BE_POSTED = "mustbeposted";
  public static final String ERROR_NO_SUCH_PAGE_ID = "nosuchpageid";
  public static final String ERROR_NO_SUCH_RC_ID = "nosuchrcid";
  public static final String ERROR_NO_SUCH_REV_ID = "nosuchrevid";
  public static final String ERROR_NO_SUCH_USER = "nosuchuser";
  public static final String ERROR_PERMISSION_DENIED = "permissiondenied";
  public static final String ERROR_PROTECTED_NAMESPACE = "protectednamespace";
  public static final String ERROR_PROTECTED_NAMESPACE_INTERFACE = "protectednamespace-interface";
  public static final String ERROR_PROTECTED_PAGE = "protectedpage";
  public static final String ERROR_RATE_LIMITED = "ratelimited";
  public static final String ERROR_READ_ONLY = "readonly";
  public static final String ERROR_UNKNOWN_ERROR = "unknownerror";
  public static final String ERROR_UNSUPPORTED_NAMESPACE = "unsupportednamespace";

  /**
   * 
   */
  private static final long serialVersionUID = 6413874942788957653L;

  /**
   * Error code. 
   */
  private final String code;

  /**
   * Constructor.
   */
  public APIException() {
    super();
    code = null;
  }

  /**
   * @param message Exception message.
   */
  public APIException(String message) {
    super(message);
    code = null;
  }

  /**
   * @param message Exception message.
   * @param code Error code.
   */
  public APIException(String message, String code) {
    super(message);
    this.code = code;
  }

  /**
   * @param cause Exception cause.
   */
  public APIException(Throwable cause) {
    super(cause);
    code = null;
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   */
  public APIException(String message, Throwable cause) {
    super(message, cause);
    code = null;
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   * @param code Error code.
   */
  public APIException(String message, Throwable cause, String code) {
    super(message, cause);
    this.code = code;
  }

  /**
   * @return Error code.
   */
  public String getErrorCode() {
    return code;
  }
}
