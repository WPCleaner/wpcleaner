/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
   * HTTP status.
   */
  private final int httpStatus;

  /**
   * Constructor.
   */
  public APIException() {
    super();
    this.code = null;
    this.httpStatus = -1;
  }

  /**
   * @param message Exception message.
   */
  public APIException(String message) {
    super(message);
    this.code = null;
    this.httpStatus = -1;
  }

  /**
   * @param message Exception message.
   * @param code Error code.
   */
  public APIException(String message, String code) {
    super(message);
    this.code = code;
    this.httpStatus = -1;
  }

  /**
   * @param message Exception message.
   * @param httpStatus HTTP status.
   */
  public APIException(String message, int httpStatus) {
    super(message);
    this.code = null;
    this.httpStatus = httpStatus;
  }

  /**
   * @param cause Exception cause.
   */
  public APIException(Throwable cause) {
    super(cause);
    this.code = null;
    this.httpStatus = -1;
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   */
  public APIException(String message, Throwable cause) {
    super(message, cause);
    this.code = null;
    this.httpStatus = -1;
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   * @param code Error code.
   */
  public APIException(String message, Throwable cause, String code) {
    super(message, cause);
    this.code = code;
    this.httpStatus = -1;
  }

  /**
   * @return Error code.
   */
  public String getErrorCode() {
    return code;
  }

  /**
   * @return HTTP status.
   */
  public int getHttpStatus() {
    return httpStatus;
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
      final Logger log = LoggerFactory.getLogger(MediaWikiAPI.class);
      if (log != null) {
        log.warn("Waiting after error '" + code + "'");
      }
      result.waitForRetry();
    }
  }
}
