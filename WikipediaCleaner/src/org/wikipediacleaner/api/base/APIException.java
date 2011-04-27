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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.impl.MediaWikiAPI;


/**
 * Generic API Exception.
 */
public class APIException extends Exception {

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

  /**
   * @return Query result.
   */
  public EnumQueryResult getQueryResult() {
    return EnumQueryResult.getEnumByCode(code);
  }

  /**
   * @return Should we retry the call ?
   */
  public boolean shouldRetry() {
    EnumQueryResult result = getQueryResult();
    return (result != null) ? result.shouldRetry() : false;
  }

  /**
   * @return Maximum number of retry attempts.
   */
  public int getMaxRetry() {
    EnumQueryResult result = getQueryResult();
    return (result != null) ? result.getMaxRetry() : 0;
  }

  /**
   * Wait for retry.
   */
  public void waitForRetry() {
    EnumQueryResult result = getQueryResult();
    if (result != null) {
      final Log log = LogFactory.getLog(MediaWikiAPI.class);
      if (log != null) {
        log.warn("Waiting after error '" + code + "'");
      }
      result.waitForRetry();
    }
  }
}
